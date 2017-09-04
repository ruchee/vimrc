<?php
namespace PHPCD;

use PhpParser\NodeVisitor\NameResolver;
use PhpParser\Node;


class Parser
{
    public static function getParentAndInterfaces($path)
    {
        $parser = (new \PhpParser\ParserFactory)->create(\PhpParser\ParserFactory::PREFER_PHP7);

        $traverser = new \PhpParser\NodeTraverser;
        $traverser->addVisitor(new \PhpParser\NodeVisitor\NameResolver);

        $visitor = new class extends \PhpParser\NodeVisitorAbstract
        {
            public $classes = [];

            public function leaveNode(Node $node)
            {
                if ($node instanceof Node\Stmt\Class_ && isset($node->namespacedName)) {
                    $this->classes[(string)$node->namespacedName] = [
                        'implements' => array_map(function ($name) { return (string) $name; }, $node->implements),
                        'extends' => $node->extends,
                    ];
                }
            }
        };

        $traverser->addVisitor($visitor);

        try {
            $stmts = $parser->parse(file_get_contents($path));
            $stmts = $traverser->traverse($stmts);
        } catch (\PhpParser\Error $e) {
            // pass
        } catch (\ErrorException $e) {
            // pass
        }

        return $visitor->classes;
    }
}
