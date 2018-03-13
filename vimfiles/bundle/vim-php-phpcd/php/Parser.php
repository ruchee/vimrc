<?php
namespace PHPCD;

use PhpParser\NodeVisitor\NameResolver;
use PhpParser\NodeTraverser;

class Parser
{
    public static function getClassName($path)
    {
        $visitor = new ClassNameVisitor;

        self::visit($path, $visitor);

        return $visitor->name;
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
     *   'name' => 'c',
     * ]
     */
    public static function getClassNameEx($path)
    {
        $visitor = new ClassNameExVisitor;

        self::visit($path, $visitor);

        return (array)$visitor;
    }

    private static function visit($path, $visitor)
    {
        $parser = (new \PhpParser\ParserFactory)->create(\PhpParser\ParserFactory::PREFER_PHP7);

        $traverser = new \PhpParser\NodeTraverser;
        $traverser->addVisitor(new \PhpParser\NodeVisitor\NameResolver);
        $traverser->addVisitor($visitor);

        try {
            $stmts = $parser->parse(file_get_contents($path));
            $stmts = $traverser->traverse($stmts);
        } catch (\Throwable $ignore) {
        }
    }
}
