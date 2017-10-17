<?php
namespace PHPCD;

use Psr\Log\LoggerInterface as Logger;
use Lvht\MsgpackRpc\Server as RpcServer;
use Lvht\MsgpackRpc\Handler as RpcHandler;

class PHPCD implements RpcHandler
{
    const MATCH_SUBSEQUENCE = 'match_subsequence';
    const MATCH_HEAD        = 'match_head';

    private $matchType;
    private $disable_modifier;

    /**
     * @var Logger
     */
    private $logger;

    /**
     * @var RpcServer
     */
    private $server;

    private $root;

    public function __construct($root, Logger $logger, $disable_modifier = 0, $match_type = self::MATCH_HEAD)
    {
        $this->logger = $logger;
        $this->root = $root;
        $this->disable_modifier = $disable_modifier;
    }

    public function setServer(RpcServer $server)
    {
        $this->server = $server;
    }

    /**
     * Set type of matching
     *
     * @param string $matchType
     */
    public function setMatchType($matchType)
    {
        if ($matchType !== self::MATCH_SUBSEQUENCE && $matchType !== self::MATCH_HEAD) {
            throw new \InvalidArgumentException('Wrong match type');
        }

        $this->matchType = $matchType;
    }

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
    public function info($class_name, $pattern, $static_mode = 'both', $public_only = true)
    {
        if ($class_name) {
            $static_mode = $this->translateStaticMode($static_mode);
            return $this->classInfo($class_name, $pattern, $static_mode, $public_only);
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
     *
     * @return [path, line]
     */
    public function location($class_name, $method_name = null)
    {
        try {
            if ($class_name) {
                $reflection = new \ReflectionClass($class_name);

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
                $reflection = new \ReflectionFunction($method_name);
            }

            return [$reflection->getFileName(), $reflection->getStartLine()];
        } catch (\ReflectionException $e) {
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

    private function getConstPath($const_name, \ReflectionClass $reflection)
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

            return $this->docClass($class_name, $name, $is_method);
        } catch (\ReflectionException $e) {
            $this->logger->debug($e->getMessage());
            return [null, null];
        }
    }

    private function docFunction($name, $path)
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

        $reflection = new \ReflectionFunction($name);
        $doc = $reflection->getDocComment();
        $path = $reflection->getFileName();

        return [$path, $this->clearDoc($doc)];
    }

    private function docClass($class_name, $name, $is_method)
    {
        $reflection_class = new \ReflectionClass($class_name);
        $reflection = null;

        if ($is_method) {
            if ($reflection_class->hasMethod($name)) {
                $reflection = $reflection_class->getMethod($name);
            } else {
                $class_doc = $this->getAllClassDocComments($reflection_class);
                $pattern = '/@method\s+(?<static>static)?\s*(?<type>\S+)\s+?'.$name.'/mi';

                return $this->matchPatternToDoc($pattern, $class_doc);
            }
        } else {
            if ($reflection_class->hasProperty($name)) {
                $reflection = $reflection_class->getProperty($name);
            } else {
                $class_doc = $this->getAllClassDocComments($reflection_class);
                $pattern = '/@property(|-read|-write)\s+(?<type>\S+)\s+\$?'.$name.'/mi';

                return $this->matchPatternToDoc($pattern, $class_doc);
            }
        }

        if (!$reflection) {
            return ['', ''];
        }

        $doc = $reflection->getDocComment();

        if ($is_method && preg_match('/@inheritDoc/', $doc)) {
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
     * Matches the give pattern to the DocComments provided
     * Expects $docs to be an array with the file name as key
     *
     * @param string $pattern
     * @param array $docs
     *
     * @return array
     */
    private function matchPatternToDoc($pattern, $docs)
    {
        foreach ($docs as $file => $doc) {
            $has_pseudo_method = preg_match($pattern, $doc, $matches);
            if ($has_pseudo_method) {
                return [$file, '@var '.$matches['type']];
            }
        }

        return ['', ''];
    }

    /**
     * Get the origin method reflection the inherited docComment belongs to.
     *
     * @param $reflection_class \ReflectionClass
     * @param $name string
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
            if (preg_match('/@inheritDoc/', $doc)) {
                $reflection_method = $this->getInheritDoc($parent_class, $method_name);
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
     *   ]
     * ]
     */
    public function nsuse($path)
    {
        $use_pattern = '/^use\s+((?<type>(constant|function)) )?(?<left>[\\\\\w]+\\\\)?({)?(?<right>[\\\\,\w\s]+)(})?\s*;$/';
        $alias_pattern = '/(?<suffix>[\\\\\w]+)(\s+as\s+(?<alias>\w+))?/';
        $class_pattern = '/^\s*\b((((final|abstract)\s+)?class)|interface|trait)\s+(?<class>\S+)/i';

        $s = [
            'namespace' => '',
            'imports' => [
                '@' => '',
                // empty array will be enoded to "[]" by json
                // so when there is no import we need convert
                // the empty array into stdobj
                // which will be encoded to "{}" by json
                // however the msgpack used by neovim does not allowed dictionary
                // with empty key. so we have no choice but fill import some
                // value to ensure none empty.
            ],
            'class' => '',
        ];

        if (!file_exists($path)) {
            return $s;
        }

        $file = new \SplFileObject($path);

        $_line = '';
        foreach ($file as $line) {
            if (preg_match($class_pattern, $line, $matches)) {
                $s['class'] = $matches['class'];
                break;
            }

            $_line .= trim($line);
            if (!($_line && preg_match('/;$/', $_line))) {
                continue;
            }

            $line = $_line;
            $_line = '';

            if (strpos($line, '<?php') === 0) {
                $line = substr($line, 5);
            }

            if (preg_match('/(<\?php)?\s*namespace\s+(.*);$/', $line, $matches)) {
                $s['namespace'] = $matches[2];
            } elseif (strtolower(substr($line, 0, 3) == 'use')) {
                if (preg_match($use_pattern, $line, $use_matches) && !empty($use_matches)) {
                    $expansions = array_map('self::trim', explode(',', $use_matches['right']));

                    foreach ($expansions as $expansion) {
                        if (preg_match($alias_pattern, $expansion, $expansion_matches) && !empty($expansion_matches)) {
                            $suffix = $expansion_matches['suffix'];

                            if (empty($expansion_matches['alias'])) {
                                $suffix_parts = explode('\\', $suffix);
                                $alias = array_pop($suffix_parts);
                            } else {
                                $alias = $expansion_matches['alias'];
                            }
                        }

                        /** empty type means import of some class **/
                        if (empty($use_matches['type']) || $use_matches['type'] == 'function') {
                            $s['imports'][$alias] = $use_matches['left'] . $suffix;
                        }
                    }
                }
            }
        }

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
        if (version_compare(PHP_VERSION, '7.0.0') >= 0) {
            $type = $this->typeByReturnType($class_name, $name, $path);
            if ($type) {
                return [$type];
            }
        }

        list($path, $doc) = $this->doc($class_name, $name, $path);
        return $this->typeByDoc($path, $doc, $class_name);
    }

    /**
     * Fetch class attribute's type by @var annotation
     *
     * @return [type1, type2, ...]
     */
    public function proptype($class_name, $name)
    {
        list($path, $doc) = $this->doc($class_name, $name, false);
        $types = $this->typeByDoc($path, $doc, $class_name);

        return $types;
    }

    private function typeByReturnType($class_name, $name, $path)
    {
        try {
            if ($class_name) {
                $reflection = new \ReflectionClass($class_name);
                $reflection = $reflection->getMethod($name);
            } else {
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

                $reflection = new \ReflectionFunction($name);
            }
            $type = (string) $reflection->getReturnType();

            if (strtolower($type) == 'self') {
                $type = $class_name;
            }

            return $type;
        } catch (\ReflectionException $e) {
            $this->logger->debug((string) $e);
        }
    }

    private function typeByDoc($path, $doc, $class_name)
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

    private function classInfo($class_name, $pattern, $is_static, $public_only)
    {
        try {
            $reflection = new \PHPCD\Reflection\ReflectionClass($class_name);
            $items = [];

            if (false !== $is_static) {
                foreach ($reflection->getConstants() as $name => $value) {
                    if (!$pattern || $this->matchPattern($pattern, $name)) {
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

            $methods = $reflection->getAvailableMethods($is_static, $public_only);

            foreach ($methods as $method) {
                $info = $this->getMethodInfo($method, $pattern);
                if ($info) {
                    $items[] = $info;
                }
            }

            $properties = $reflection->getAvailableProperties($is_static, $public_only);

            foreach ($properties as $property) {
                $info = $this->getPropertyInfo($property, $pattern);
                if ($info) {
                    $items[] = $info;
                }
            }

            $pseudo_items = $this->getPseudoMethods($reflection);
            $items = array_merge($items, $pseudo_items);

            $pseudo_items = $this->getPseudoProperties($reflection);
            $items = array_merge($items, $pseudo_items);

            return $items;
        } catch (\ReflectionException $e) {
            $this->logger->debug($e->getMessage());
            return [];
        }
    }
    /**
     * Get class DocComment methods, from parents as well
     *
     * @param \ReflectionClass
     *
     * @return string
     *
     * @author yourname
     */
    private function getAllClassDocComments(\ReflectionClass $reflection)
    {
        $doc = [];
        do {
            $file_name = $reflection->getFileName();
            $doc[$file_name] = $reflection->getDocComment();
            $reflection = $reflection->getParentClass();
        } while ($reflection); // gets the parents properties too

        return $doc;
    }

    public function getPseudoProperties(\ReflectionClass $reflection)
    {
        $doc = $this->getAllClassDocComments($reflection);
        $all_docs = '';
        foreach ($doc as $class_doc) {
            $all_docs .= $class_doc;
        }

        $has_doc = preg_match_all('/@property(|-read|-write)\s+(?<types>\S+)\s+\$?(?<names>[a-zA-Z0-9_$]+)/mi', $all_docs, $matches);
        if (!$has_doc) {
            return [];
        }

        $items = [];
        foreach ($matches['names'] as $idx => $name) {
            $items[] = [
                'word' => $name,
                'abbr' => $this->disable_modifier ? $name : sprintf('%3s %s', '+', $name),
                'info' => $matches['types'][$idx],
                'kind' => 'p',
                'icase' => 1,
            ];
        }

        return $items;
    }

    public function getPseudoMethods(\ReflectionClass $reflection)
    {
        $doc = $this->getAllClassDocComments($reflection);
        $all_docs = '';
        foreach ($doc as $class_doc) {
            $all_docs .= $class_doc;
        }

        $has_doc = preg_match_all('/@method\s+(?<statics>static)?\s*(?<types>\S+)\s+(?<names>[a-zA-Z0-9_$]+)\((?<params>.*)\)/mi', $all_docs, $matches);
        if (!$has_doc) {
            return [];
        }

        $items = [];
        foreach ($matches['names'] as $idx => $name) {
            preg_match_all('/\$[a-zA-Z0-9_]+/mi', $matches['params'][$idx], $params);

            if ($this->disable_modifier) {
                $abbr = sprintf("%s(%s)", $name, join(', ', end($params)));
            } else {
                $abbr = sprintf("%3s %s(%s)", '+', $name, join(', ', end($params)));
            }

            $items[] = [
                'word' => $name,
                'abbr' => $abbr,
                'info' => $matches['types'][$idx],
                'kind' => 'f',
                'icase' => 1,
            ];
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
            if ($pattern && strpos($name, $pattern) !== 0) {
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

    private function getFunctionInfo($name, $pattern = null)
    {
        if ($pattern && strpos($name, $pattern) !== 0) {
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

    private function getPropertyInfo($property, $pattern)
    {
        $name = $property->getName();
        if ($pattern && !$this->matchPattern($pattern, $name)) {
            return null;
        }

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
    private function formatParamInfo(\ReflectionParameter $param)
    {
        /* @var ReflectionClass|null $hintedClass */
        $hintedClass = $param->getClass();
        $paramString = '';
        if ($hintedClass) {
            $paramString .= $hintedClass->getName() . ' ';
        } elseif ($param->hasType()) {
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
     * @param \ReflectionMethod $param
     * @return string
     */
    private function getExtraMethodInfo(\ReflectionMethod $method)
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
            "%s %s%s%s(\n%s)%s\n%s",
            $visibility,
            $declaringClass,
            $accessMode,
            $name,
            count($paramsInfo)
                ? '    ' . join(",\n    ", $paramsInfo) . "\n"
                : '',
            $returnType,
            $docblock
        );
    }

    private function getMethodInfo(\ReflectionMethod $method, $pattern = null)
    {
        $name = $method->getName();
        if ($pattern && !$this->matchPattern($pattern, $name)) {
            return null;
        }

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

    /**
     * @return bool
     */
    private function matchPattern($pattern, $fullString)
    {
        if (!$pattern) {
            return true;
        }

        switch ($this->matchType) {
            case self::MATCH_SUBSEQUENCE:
                // @TODO Case sensitivity of matching should be probably configurable
                $modifiers = 'i';
                $regex = sprintf('/%s/%s', implode('.*', array_map('preg_quote', str_split($pattern))), $modifiers);

                return (bool)preg_match($regex, $fullString);
            default:
                return stripos($fullString, $pattern) === 0;
        }
    }

    /**
     *
     * @return array
     */
    private function getModifierSymbols()
    {
        return $this->modifier_symbols;
    }

    private function getModifiers($reflection)
    {
        if ($this->disable_modifier) {
            return '';
        }

        $signs = '';

        $modifiers = $reflection->getModifiers();
        $symbols = $this->getModifierSymbols();

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

    public function keyword()
    {
        $keywords = require __DIR__.'/keyword.php';
        return array_map(function ($keyword) {
            return [
                'word' => $keyword,
                'abbr' => $keyword,
                'info' => '',
                'kind' => 'd',
                'icase' => 1,
            ];
        }, $keywords);
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
            if ($this->matchPattern($pattern, $name)) {
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
}
