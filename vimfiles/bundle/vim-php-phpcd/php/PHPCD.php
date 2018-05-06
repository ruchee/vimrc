<?php
namespace PHPCD;

use Psr\Log\LoggerInterface as Logger;
use Lvht\MsgpackRpc\Server as RpcServer;
use Lvht\MsgpackRpc\Handler as RpcHandler;

use Roave\BetterReflection\BetterReflection;
use Roave\BetterReflection\Reflector\ClassReflector;
use Roave\BetterReflection\Reflector\FunctionReflector;
use Roave\BetterReflection\SourceLocator\Type\SingleFileSourceLocator;
use Roave\BetterReflection\SourceLocator\Type\PhpInternalSourceLocator;
use Roave\BetterReflection\SourceLocator\Type\AutoloadSourceLocator;
use Roave\BetterReflection\SourceLocator\Type\AggregateSourceLocator;

class PHPCD implements RpcHandler
{
    private $disable_modifier;

    /**
     * @var Matcher\Matcher
     */
    private $matcher;

    private $logger;

    private $server;

    private $root;

    /**
     *  @param array Map between modifier numbers and displayed symbols
     */
    private $modifier_symbols = [
        \ReflectionMethod::IS_FINAL      => '!',
        \ReflectionMethod::IS_PRIVATE    => '-',
        \ReflectionMethod::IS_PROTECTED  => '#',
        \ReflectionMethod::IS_PUBLIC     => '+',
        \ReflectionMethod::IS_STATIC     => '@'
    ];

    private $primitive_types = [
        'array'    => 1,
        'bool'     => 1,
        'callable' => 1,
        'double'   => 1,
        'float'    => 1,
        'int'      => 1,
        'mixed'    => 1,
        'null'     => 1,
        'object'   => 1,
        'resource' => 1,
        'scalar'   => 1,
        'string'   => 1,
        'void'     => 1,
    ];

    public function __construct($root, Logger $logger, $disable_modifier = 0)
    {
        $this->logger = $logger;
        $this->root = $root;
        $this->disable_modifier = $disable_modifier;
        $this->matcher = Matcher\Factory::make('prefix');
    }

    public function setServer(RpcServer $server)
    {
        $this->server = $server;
    }

    /**
     * Set type of matching
     *
     * @param string $type matcher type, support prefix, substr, fuzzy
     */
    public function setMatchType($type)
    {
        $this->matcher = Matcher\Factory::make($type);
    }

    /**
     * @param string $mode
     * @return bool|null
     */
    private function translateStaticMode($mode)
    {
        $map = [
            'both'           => null,
            'only_nonstatic' => false,
            'only_static'    => true
        ];

        return isset($map[$mode]) ? $map[$mode] : null;
    }

    /**
     * Fetch the completion list.
     *
     * If both $class_name and $pattern are setted, it will list the class's
     * methods, constants, and properties, filted by pattern.
     *
     * If only $pattern is setted, it will list all the defined function
     * (including the PHP's builtin function', filted by pattern.
     *
     * @var string $class_name
     * @var string $pattern
     * @var string $static_mode see translateStaticMode method
     * @var bool $public_only
     */
    public function info($class_name, $pattern, $static_mode, $public_only, $path)
    {
        if ($class_name) {
            $static_mode = $this->translateStaticMode($static_mode);
            return $this->classInfo($class_name, $pattern, $static_mode, $public_only, $path);
        }

        if ($pattern) {
            return $this->functionOrConstantInfo($pattern);
        }

        return [];
    }

    /**
     * Fetch function or class method's source file path
     * and their defination line number.
     *
     * @param string $class_name class name
     * @param string $method_name method or function name
     * @param string $path current source file
     *
     * @return [path, line]
     */
    public function location($class_name, $method_name, $path)
    {
        try {
            if ($class_name) {
                $reflection = $this->reflectClass($class_name, $path);

                if ($reflection->hasMethod($method_name)) {
                    $reflection = $reflection->getMethod($method_name);
                } elseif ($reflection->hasConstant($method_name)) {
                    // 常量则返回 [ path, 'const CONST_NAME' ]
                    return [$this->getConstPath($method_name, $reflection), 'const ' . $method_name];
                } elseif ($reflection->hasProperty($method_name)) {
                    $line = $this->getPropertyDefLine($reflection, $method_name);
                    return [$reflection->getFileName(), $line];
                }
            } else {
                $reflection = $this->reflectFunction($method_name, $path);
            }

            return [$reflection->getFileName(), $reflection->getStartLine()];
        } catch (\Exception $e) {
            return ['', null];
        }
    }

    private function getPropertyDefLine($classReflection, $property)
    {
        $class = new \SplFileObject($classReflection->getFileName());
        $class->seek($classReflection->getStartLine());

        $pattern = '/(private|protected|public|var)\s\$' . $property . '/x';
        foreach ($class as $line => $content) {
            if (preg_match($pattern, $content)) {
                return $line + 1;
            }
        }
        return $classReflection->getStartLine();
    }

    /**
     * @param string $const_name
     * @param \ReflectionClass $reflection
     */
    private function getConstPath($const_name, $reflection)
    {
        $origin = $path = $reflection->getFileName();
        $origin_reflection = $reflection;

        while ($reflection = $reflection->getParentClass()) {
            if ($reflection->hasConstant($const_name)) {
                $path = $reflection->getFileName();
            } else {
                break;
            }
        }

        if ($origin === $path) {
            $interfaces = $origin_reflection->getInterfaces();
            foreach ($interfaces as $interface) {
                if ($interface->hasConstant($const_name)) {
                    $path = $interface->getFileName();
                    break;
                }
            }
        }

        return $path;
    }

    /**
     * Fetch function, class method or class attribute's docblock
     *
     * @param string $class_name for function set this args to empty
     * @param string $name
     */
    private function doc($class_name, $name, $is_method = true, $path = null)
    {
        try {
            if (!$class_name) {
                return $this->docFunction($name, $path);
            }

            return $this->docClass($class_name, $name, $is_method, $path);
        } catch (\Exception $e) {
            $this->logger->debug((string) $e);
            return [null, null];
        }
    }

    private function docFunction($name, $path)
    {
        $nsuse = $this->nsuse($path);

        if (isset($nsuse['alias'][$name])) {
            $_name = $nsuse['alias'][$name];
        } else {
            $_name = $nsuse['namespace'].'\\'.$name;
        }

        if (function_exists($_name)) {
            $name = $_name;
        }

        $reflection = $this->reflectFunction($name, $path);

        $doc = $reflection->getDocComment();
        $path = $reflection->getFileName();

        return [$path, $this->clearDoc($doc)];
    }

    private function docClass($class_name, $name, $is_method, $path)
    {
        $reflection_class = $this->reflectClass($class_name, $path);
        $reflection = null;

        if ($is_method) {
            if ($reflection_class->hasMethod($name)) {
                $reflection = $reflection_class->getMethod($name);
            } else {
                $methods = $this->getPseudoMethods($reflection_class, $name);

                if (!isset($methods[$name])) {
                    return ['', ''];
                }

                $method = $methods[$name];

                return [$method['file'], '@var '.$method['type']];
            }
        } else {
            if ($reflection_class->hasProperty($name)) {
                $reflection = $reflection_class->getProperty($name);
            } else {
                $properties = $this->getPseudoProperties($reflection_class, $name);

                if (!isset($properties[$name])) {
                    return ['', ''];
                }

                $property = $properties[$name];

                return [$property['file'], '@var '.$property['type']];
            }
        }

        if (!$reflection) {
            return ['', ''];
        }

        $doc = $reflection->getDocComment();

        if ($is_method && preg_match('/{?@{?inheritdoc}?/i', $doc)) {
            $reflection = $this->getReflectionFromInheritDoc($reflection_class, $name);
            $doc = $reflection->getDocComment();
        }

        if (preg_match('/@(return|var)\s+static/i', $doc)) {
            $path = $reflection_class->getFileName();
        } else {
            $path = $reflection->getDeclaringClass()->getFileName();
        }

        return [$path, $this->clearDoc($doc)];
    }

    /**
     * Get the origin method reflection the inherited docComment belongs to.
     *
     * @param \ReflectionClass $reflection_class
     * @param string $name
     *
     * @return \ReflectionClass
     */
    private function getReflectionFromInheritDoc($reflection_class, $method_name)
    {
        $interfaces = $reflection_class->getInterfaces();

        foreach ($interfaces as $interface) {
            if ($interface->hasMethod($method_name)) {
                return $interface->getMethod($method_name);
            }
        }

        $reflection_method = $reflection_class->getMethod($method_name);
        $parent_class = $reflection_class->getParentClass();

        if ($parent_class) {
            $reflection_method = $parent_class->getMethod($method_name);
            $doc = $reflection_method->getDocComment();
            if (preg_match('/{?@{?inheritdoc}?/i', $doc)) {
                $reflection_method = $this->getReflectionFromInheritDoc($parent_class, $method_name);
            }
        }

        return $reflection_method;
    }

    /**
     * Fetch the php script's namespace and imports(by use) list.
     *
     * @param string $path the php scrpit path
     *
     * @return [
     *   'namespace' => 'ns',
     *   'imports' => [
     *     'alias1' => 'fqdn1',
     *   ],
     *   'class' => '',
     *   'start_line' => 1,
     *   'end_line' => 5,
     * ]
     */
    public function nsuse($path)
    {
        $s = Parser::getClassNameEx($path);

        if (!$s['imports']) {
            // empty array will be enoded to "[]" by json
            // so when there is no import we need convert
            // the empty array into stdobj
            // which will be encoded to "{}" by json
            // however the msgpack used by neovim does not allowed dictionary
            // with empty key. so we have no choice but fill import some
            // value to ensure none empty.
            $s['imports']['@'] = '';
        }

        $s['namespace'] = (string) $s['namespace'];
        $s['class'] = (string) $s['name'];
        unset($s['name']);

        return $s;
    }

    private static function trim($str)
    {
        return trim($str, "\t\n\r\0\x0B\\ ");
    }

    /**
     * Fetch the function or class method return value's type
     *
     * For PHP7 or newer version, it tries to use the return type gramar
     * to fetch the real return type.
     *
     * For PHP5, it use the docblock's return or var annotation to fetch
     * the type.
     *
     * @return [type1, type2]
     */
    public function functype($class_name, $name, $path)
    {
        $method = $class_name ? "$class_name::$name" : $name;
        $method = trim($method, '\\');
        $method_return = require __DIR__.'/method_return.php';

        if (isset($method_return[$method])) {
            return [$method_return[$method]];
        }

        if (version_compare(PHP_VERSION, '7.0.0') >= 0) {
            $type = $this->typeByReturnType($class_name, $name, $path);
            if ($type) {
                return [$type];
            }
        }

        list($path, $doc) = $this->doc($class_name, $name, true, $path);
        return $this->typeByDoc($path, $doc);
    }

    /**
     * Fetch the function or class method parameter type by type hint or docblock
     *
     * @return [type1, type2]
     */
    public function argtype($class_name, $func_name, $name, $path)
    {
        if ($name && $name[0] === '$') {
            $name = substr($name, 1);
        }

        $type = $this->argTypeByHint($class_name, $func_name, $name, $path);
        if ($type && !$type->isBuiltin()) {
            return ["\\".$type];
        }

        list($path, $doc) = $this->doc($class_name, $func_name, true, $path);
        return $this->argTypeByDoc($path, $doc, $name);
    }

    private function argTypeByHint($class_name, $func_name, $name, $path)
    {
        try {
            if ($class_name) {
                $reflection = $this->reflectClass($class_name, $path);
                $reflection = $reflection->getMethod($func_name);
            } else {
                $reflection = $this->reflectFunction($func_name, $path);
            }

            /** @var \ReflectionMethod $reflection */
            foreach ($reflection->getParameters() as $parameter) {
                /** @var \ReflectionParameter $parameter */
                if ($parameter->getName() === $name) {
                    return $parameter->getType();
                }
            }
        } catch (\Exception $e) {
            $this->logger->debug((string) $e);
        }
    }

    private function argTypeByDoc($path, $doc, $name)
    {
        $has_doc = preg_match('/@param\s+(\S+)\s+\$'.$name.'/m', $doc, $matches);
        if ($has_doc) {
            return $this->fixRelativeType($path, explode('|', $matches[1]));
        }

        return [];
    }

    /**
     * Fetch class attribute's type by @var annotation
     *
     * @return [type1, type2, ...]
     */
    public function proptype($class_name, $name, $path)
    {
        list($path, $doc) = $this->doc($class_name, $name, false, $path);
        $types = $this->typeByDoc($path, $doc);

        if (!$types) {
            $types = $this->proptypeByMethod($class_name, $name, '__construct', $path);
        }

        if (!$types) {
            $setter = 'set'.ucfirst($name);
            $types = $this->proptypeByMethod($class_name, $name, $setter, $path);
        }

        return $types;
    }

    private function proptypeByMethod($class_name, $name, $method, $path)
    {
        try {
            $reflection = $this->reflectClass($class_name, $path);
        } catch (\Exception $e) {
            return [];
        }

        if (!$reflection->hasMethod($method)) {
            return [];
        }

        $constructor = $reflection->getMethod($method);

        if (!$constructor) {
            return [];
        }

        $path = $constructor->getFileName();
        $start_line = $constructor->getStartLine();
        $end_line = $constructor->getEndLine();

        $lines = file($path);
        $body_lines = array_slice($lines, $start_line, $end_line - $start_line);

        foreach ($body_lines as $line) {
            if (preg_match('/(\\$this'."-\\>|self::\\$)$name\\s*=\\s*(new\s+|\\$)([\\w\\\\]+)\b/i", $line, $matches)) {
                if ($matches[2][0] == 'n') { // $this->foo = new Foo;
                    $types = [$matches[3]];
                    return $this->fixRelativeType($path, $types);
                }

                $value_name = $matches[3];
                /** @var \ReflectionParameter $parameter */
                foreach ($constructor->getParameters() as $parameter) {
                    if ($parameter->getName() === $value_name) {
                        $value_type = (string) $parameter->getType();
                        return [$value_type];
                    }
                }
            }
        }

        return [];
    }

    private function typeByReturnType($class_name, $name, $path)
    {
        try {
            if ($class_name) {
                $reflection = $this->reflectClass($class_name, $path);
                $reflection = $reflection->getMethod($name);
            } else {
                $reflection = $this->reflectFunction($name, $path);
            }
            $type = (string) $reflection->getReturnType();

            if (strtolower($type) == 'self') {
                $type = $class_name;
            }

            return $type;
        } catch (\Exception $e) {
            $this->logger->debug((string) $e);
        }
    }

    private function typeByDoc($path, $doc)
    {
        $has_doc = preg_match('/@(return|var)\s+(\S+)/m', $doc, $matches);
        if ($has_doc) {
            return $this->fixRelativeType($path, explode('|', $matches[2]));
        }

        return [];
    }

    private function fixRelativeType($path, $names)
    {
        $nsuse = null;

        $types = [];
        foreach ($names as $type) {
            if (isset($this->primitive_types[$type])) {
                continue;
            }

            if (!$nsuse && $type[0] != '\\') {
                $nsuse = $this->nsuse($path);
            }

            if (in_array(strtolower($type) , ['static', '$this', 'self'])) {
                $type = $nsuse['namespace'] . '\\' . $nsuse['class'];
            } elseif ($type[0] != '\\') {
                $parts = explode('\\', $type);
                $alias = array_shift($parts);
                if (isset($nsuse['imports'][$alias])) {
                    $type = $nsuse['imports'][$alias];
                    if ($parts) {
                        $type = $type . '\\' . join('\\', $parts);
                    }
                } else {
                    $type = $nsuse['namespace'] . '\\' . $type;
                }
            }

            if ($type) {
                if ($type[0] != '\\') {
                    $type = '\\' . $type;
                }
                $types[] = $type;
            }
        }

        return self::arrayUnique($types);
    }

    private static function arrayUnique($array)
    {
        $_ = [];
        foreach ($array as $a) {
            $_[$a] = 1;
        }

        return array_keys($_);
    }

    private function reflectFunction($name, $path)
    {
        $nsuse = $this->nsuse($path);

        if (isset($nsuse['alias'][$name])) {
            $_name = $nsuse['alias'][$name];
            if (function_exists($_name)) {
                $name = $_name;
            }
        } else {
            $_name = $nsuse['namespace'].'\\'.$name;
            if (function_exists($_name)) {
                $name = $_name;
            }
        }

        if (function_exists($name)) {
            $reflection = new \ReflectionFunction($name);
        } else {
            $ast_locator = (new BetterReflection())->astLocator();
            $source_locator = new SingleFileSourceLocator($path, $ast_locator);
            $class_reflector = new ClassReflector($source_locator);
            $reflector = new FunctionReflector($source_locator, $class_reflector);
            $reflection = $reflector->reflect($name);
        }

        return $reflection;
    }

    private function reflectClass($class_name, $path)
    {
        if (class_exists($class_name)) {
            $reflection = new \ReflectionClass($class_name);
        } else {
            $ast_locator = (new BetterReflection())->astLocator();
            $source_locator = new SingleFileSourceLocator($path, $ast_locator);
            $internal_locator = new PhpInternalSourceLocator($ast_locator);
            $autoload_locator = new AutoloadSourceLocator($ast_locator);
            $locator = new AggregateSourceLocator([$source_locator, $autoload_locator, $internal_locator]);
            $reflector = new ClassReflector($locator);

            $reflection = $reflector->reflect($class_name);
        }

        return $reflection;
    }

    private function classInfo($class_name, $pattern, $is_static, $public_only, $path)
    {
        $items = [];

        try {
            $reflection = $this->reflectClass($class_name, $path);

            if (false !== $is_static) {
                foreach ($this->getAvailableConstants($reflection, $pattern) as $name => $value) {
                    {
                        if (is_array($value)) {
                            $value = '[...]';
                        }

                        $items[] = [
                            'word' => $name,
                            'abbr' => sprintf(" +@ %s %s", $name, $value),
                            'kind' => 'd',
                            'icase' => 1,
                        ];
                    }
                }
            }

            $methods = $this->getAvailableMethods($reflection, $is_static, $public_only, $pattern);

            foreach ($methods as $method) {
                $items[] = $this->getMethodInfo($method);
            }

            $properties = $this->getAvailableProperties($reflection, $is_static, $public_only, $pattern);

            foreach ($properties as $property) {
                $items[] = $this->getPropertyInfo($property);
            }

            $pseudo_methods = $this->getPseudoMethods($reflection, $pattern);
            foreach ($pseudo_methods as $name => $info) {
                if ($this->disable_modifier) {
                    $abbr = sprintf("%s(%s)", $name,  $info['params']);
                } else {
                    $abbr = sprintf("%3s %s(%s)", '+', $name, $info['params']);
                }

                $items[] = [
                    'word' => $name,
                    'abbr' => $abbr,
                    'info' => $info['type'],
                    'kind' => 'f',
                    'icase' => 1,
                ];
            }

            $pseudo_properties = $this->getPseudoProperties($reflection, $pattern);
            foreach ($pseudo_properties as $name => $info) {
                $items[] = [
                    'word' => $name,
                    'abbr' => $this->disable_modifier ? $name : sprintf('%3s %s', '+', $name),
                    'info' => $info['type'],
                    'kind' => 'p',
                    'icase' => 1,
                ];
            }

            return $items;
        } catch (\Exception $e) {
            $this->logger->debug((string) $e);
        }

        return $items;
    }

    private function functionOrConstantInfo($pattern)
    {
        $items = [];
        $funcs = get_defined_functions();

        foreach ($funcs['internal'] as $func) {
            $info = $this->getFunctionInfo($func, $pattern);
            if ($info) {
                $items[] = $info;
            }
        }

        foreach ($funcs['user'] as $func) {
            $info = $this->getFunctionInfo($func, $pattern);
            if ($info) {
                $items[] = $info;
            }
        }

        return array_merge($items, $this->getConstantsInfo($pattern));
    }

    private function getConstantsInfo($pattern)
    {
        $items = [];
        foreach (get_defined_constants() as $name => $value) {
            if (!$this->matcher->match($pattern, $name)) {
                continue;
            }

            if (is_array($value)) {
                $value = '[...]';
            }
            $items[] = [
                'word' => $name,
                'abbr' => "@ $name = $value",
                'kind' => 'd',
                'icase' => 0,
            ];
        }

        return $items;
    }

    public function classes($pattern)
    {
        $items = [];
        foreach (get_declared_classes() as $name) {
            if (!$this->matcher->match($pattern, $name)) {
                continue;
            }

            $reflection = new \ReflectionClass($name);

            $item = [
                'word' => $name,
                'abbr' => "© $name",
                'kind' => 'f',
                'icase' => 0,
            ];

            if ($method = $reflection->getConstructor()) {
                $item['info'] = $this->getExtraMethodInfo($method);
            }

            $items[] = $item;
        }

        return $items;
    }

    private function getFunctionInfo($name, $pattern)
    {
        if (!$this->matcher->match($pattern, $name)) {
            return null;
        }

        $reflection = new \ReflectionFunction($name);
        $params = array_map(function ($param) {
            return $param->getName();
        }, $reflection->getParameters());

        return [
            'word' => $name,
            'abbr' => "$name(" . join(', ', $params) . ')',
            'info' => preg_replace('#/?\*(\*|/)?#','', $reflection->getDocComment()),
            'kind' => 'f',
            'icase' => 1,
        ];
    }

    private function getPropertyInfo($property)
    {
        $name = $property->getName();

        $modifier = $this->getModifiers($property);
        if ($property->getModifiers() & \ReflectionMethod::IS_STATIC) {
            $name = '$'.$name;
        }

        return [
            'word' => $name,
            'abbr' => $modifier ? sprintf("%3s %s", $modifier, $name) : $name,
            'info' => preg_replace('#/?\*(\*|/)?#', '', $property->getDocComment()),
            'kind' => 'p',
            'icase' => 1,
        ];
    }

      /**
     * @param \ReflectionParameter $param
     * @return string
     */
    private function formatParamInfo($param)
    {
        /* @var ReflectionClass|null $hintedClass */
        $hintedClass = $param->getClass();
        $paramString = '';
        if ($hintedClass) {
            $paramString .= $hintedClass->getName() . ' ';
        } elseif (method_exists($param, 'hasType') && $param->hasType()) {
            $paramString .= $param->getType() . ' ';
        }
        $paramString .= '$' . $param->getName();
        if (!$param->isDefaultValueAvailable()) {
            return $paramString;
        }
        if ($param->isDefaultValueConstant()) {
            $default = $param->getDefaultValueConstantName();
        } else {
            $default = $param->getDefaultValue();
            if (!is_string($default)) {
                $default = json_encode($default);
            }
        }
        $paramString .= " = " . $default;
        return $paramString;
    }

    /**
     * @param \ReflectionMethod $method
     * @return string
     */
    private function getExtraMethodInfo($method)
    {
        $name = $method->getName();
        $declaringClass = $method->getDeclaringClass()->getName();
        $visibility  = $method->isPrivate()   ? 'private'
                     : $method->isProtected() ? 'protected'
                     : 'public';
        $accessMode = $method->isStatic() ? "::" : "->";
        $paramsInfo = [];
        foreach ($method->getParameters() as $param) {
            $paramsInfo[] = $this->formatParamInfo($param);
        }
        $returnType = '';
        if (method_exists($method, 'getReturnType') && $method->hasReturnType()) {
            $returnType = " : " . (string)$method->getReturnType();
        }
        $docblock = $this->clearDoc($method->getDocComment());
        return sprintf(
            "%s %s%s%s(%s)%s%s",
            $visibility,
            $declaringClass,
            $accessMode,
            $name,
            join(", ", $paramsInfo),
            $returnType,
            $docblock
        );
    }

    /**
     * @param \ReflectionMethod $method
     */
    private function getMethodInfo($method)
    {
        $name = $method->getName();

        $params = array_map(function ($param) {
            return $param->getName();
        }, $method->getParameters());

        $modifier = $this->getModifiers($method);
        if ($modifier) {
            $abbr = sprintf("%3s %s(%s)", $modifier, $name, join(', ', $params));
        } else {
            $abbr = sprintf("%s(%s)", $name, join(', ', $params));
        }

        return [
            'word' => $name,
            'abbr' => $abbr,
            'info' => $this->getExtraMethodInfo($method),
            'kind' => 'f',
            'icase' => 1,
        ];
    }

    private function getModifiers($reflection)
    {
        if ($this->disable_modifier) {
            return '';
        }

        $signs = '';

        $modifiers = $reflection->getModifiers();
        $symbols = $this->modifier_symbols;

        foreach ($symbols as $number => $sign) {
            if ($number & $modifiers) {
                $signs .= $sign;
            }
        }

        return $signs;
    }

    private function clearDoc($doc)
    {
        $doc = preg_replace('/[ \t]*\* ?/m','', $doc);
        return preg_replace('#\s*\/|/\s*#','', $doc);
    }

    /**
     * generate psr4 namespace according composer.json and file path
     */
    public function psr4ns($path)
    {
        $dir = dirname($path);

        $composer_path = $this->root . '/composer.json';

        if (!is_readable($composer_path)) {
            return [];
        }

        $composer = json_decode(file_get_contents($composer_path), true);

        if (isset($composer['autoload']['psr-4'])) {
            $list = (array) $composer['autoload']['psr-4'];
        } else {
            $list = [];
        }

        if (isset($composer['autoload-dev']['psr-4'])) {
            $dev_list = (array) $composer['autoload-dev']['psr-4'];
        } else {
            $dev_list = [];
        }

        foreach ($dev_list as $namespace => $paths) {
            if (isset($list[$namespace])) {
                $list[$namespace] = array_merge((array)$list[$namespace], (array) $paths);
            } else {
                $list[$namespace] = (array) $paths;
            }
        }

        $namespaces = [];
        foreach ($list as $namespace => $paths) {
            foreach ((array)$paths as $path) {
                $path = realpath($this->root.'/'.$path);
                if (strpos($dir, $path) === 0) {
                    $sub_path = str_replace($path, '', $dir);
                    $sub_path = str_replace('/', '\\', $sub_path);
                    $sub_namespace = trim($sub_path, '\\');
                    if ($sub_namespace) {
                        $sub_namespace = '\\' . $sub_namespace;
                    }
                    $namespaces[] = trim($namespace, '\\').$sub_namespace;
                }
            }
        }

        return $namespaces;
    }

    public function keyword($pattern)
    {
        $keywords = require __DIR__.'/keyword.php';

        $items = [];
        foreach ($keywords as $keyword) {
            if ($this->matcher->match($pattern, $keyword)) {
                $items[] = [
                    'word' => $keyword,
                    'abbr' => $keyword,
                    'info' => '',
                    'kind' => 'd',
                    'icase' => 1,
                ];
            }
        }

        return $items;
    }

    public function classmap($pattern)
    {
        $use_fqdn = isset($pattern[0]) && $pattern[0] === "\\";
        $pattern = trim($pattern, "\\");
        $classmap_file = $this->root.'/vendor/composer/autoload_classmap.php';
        $defined = array_merge(get_declared_traits(),
            get_declared_interfaces(), get_declared_classes());

        if (is_readable($classmap_file)) {
            $_classmap = require $classmap_file;
            if ($_classmap) {
                $defined = array_merge($defined, array_keys($_classmap));
            }
        }

        $classmap = [];
        foreach ($defined as $name) {
            if ($this->matcher->match($pattern, $name)) {
                $classmap[] = $name;
            }
        }

        return array_map(function ($name) use($use_fqdn) {
            if ($use_fqdn) {
                $name = "\\".$name;
            }
            return [
                'word' => $name,
                'abbr' => $name,
                'info' => '',
                'kind' => 't',
                'icase' => 1,
            ];
        }, array_unique($classmap));
    }

    /**
     * @param \ReflectionClass $reflection
     * @param string $pattern
     */
    private function getAvailableConstants($reflection, $pattern)
    {
        $constants = $reflection->getConstants();

        foreach ($constants as $name => $value) {
            if (!$this->matcher->match($pattern, $name)) {
                unset($constants[$name]);
            }
        }

        return $constants;
    }

    /**
     * Get methods available for given class
     * depending on context
     *
     * @param \ReflectionClass $reflection
     * @param bool|null $static Show static|non static|both types
     * @param bool public_only restrict the result to public methods
     * @return \ReflectionMethod[]
     */
    private function getAvailableMethods($reflection, $static, $public_only, $pattern)
    {
        $methods = $reflection->getMethods();

        foreach ($methods as $key => $method) {
            if (!$this->filter($reflection, $method, $static, $public_only, $pattern)) {
                unset($methods[$key]);
            }
        }

        return $methods;
    }

    /**
     * Get properties available for given class
     * depending on context
     *
     * @param \ReflectionClass $reflection
     * @param bool|null $static Show static|non static|both types
     * @param bool public_only restrict the result to public properties
     * @return \ReflectionProperty[]
     */
    private function getAvailableProperties($reflection, $static, $public_only, $pattern)
    {
        $properties = $reflection->getProperties();

        foreach ($properties as $key => $property) {
            if (!$this->filter($reflection, $property, $static, $public_only, $pattern)) {
                unset($properties[$key]);
            }
        }

        return $properties;
    }

    /**
     * @param \ReflectionClass $reflection
     */
    private function getPseudoProperties($reflection, $pattern)
    {
        $properties = [];

        foreach ($this->getAllClassDocComments($reflection) as $file_name => $doc) {
            $has_doc = preg_match_all('/@property(|-read|-write)\s+(?<types>\S+)\s+\$?(?<names>[a-zA-Z0-9_$]+)/mi', $doc, $matches);

            if (!$has_doc) {
                continue;
            }

            foreach ($matches['names'] as $idx => $name) {
                if (!$this->matcher->match($pattern, $name)) {
                    continue;
                }

                $properties[$name] = [
                    'type' => $matches['types'][$idx],
                    'file' => $file_name,
                ];
            }
        }

        return $properties;
    }

    /**
     * @param \ReflectionClass $reflection
     */
    private function getPseudoMethods($reflection, $pattern)
    {
        $methods = [];

        foreach ($this->getAllClassDocComments($reflection) as $file_name => $doc) {
            $has_doc = preg_match_all('/@method\s+(?<statics>static)?\s*(?<types>\S+)\s+(?<names>[a-zA-Z0-9_$]+)\((?<params>.*)\)/mi', $doc, $matches);

            if (!$has_doc) {
                continue;
            }

            foreach ($matches['names'] as $idx => $name) {
                if (!$this->matcher->match($pattern, $name)) {
                    continue;
                }

                $methods[$name] = [
                    'file' => $file_name,
                    'type' => $matches['types'][$idx],
                    'params' => $matches['params'][$idx],
                ];
            }
        }

        return $methods;
    }

    /**
     * @param \ReflectionClass $reflection
     */
    private function getAllClassDocComments($reflection)
    {
        $doc = [];

        do {
            $file_name = $reflection->getFileName();
            $doc[$file_name] = $reflection->getDocComment();
            $reflection = $reflection->getParentClass();
        } while ($reflection); // gets the parents properties too

        return $doc;
    }

    private function filter($reflection, $element, $static, $public_only, $pattern)
    {
        if (!$this->matcher->match($pattern, $element->getName())) {
            return false;
        }

        if ($static !== null) {
            if ($static != $element->isStatic()) {
                return false;
            }
        }

        if ($element->isPublic()) {
            return true;
        }

        if ($public_only) {
            return false;
        }

        if ($element->isProtected()) {
            return true;
        }

        // $element is then private
        return $element->getDeclaringClass()->getName() === $reflection->getName();
    }
}
