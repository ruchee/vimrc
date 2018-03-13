<?php
namespace PHPCD;

use PhpParser\Node;
use PhpParser\NodeVisitorAbstract;
use PhpParser\NodeTraverser;

class ClassNameExVisitor extends NodeVisitorAbstract
{
    public $namespace;
    public $imports = [];
    public $name;

    public function enterNode(Node $node)
    {
        if ($node instanceof Node\Stmt\Namespace_) {
            $this->namespace = (string)$node->name;
        }

        if ($node instanceof Node\Stmt\GroupUse) {
            // @var Node\Stmt\UseUse $user
            $prefix = (string)$node->prefix;
            foreach ($node->uses as $use) {
                $alias = $use->alias;

                if ($use->type !== Node\Stmt\Use_::TYPE_CONSTANT) {
                    $name = $prefix."\\".(string)$use->name;
                    $this->imports[$alias] = $name;
                }
            }
        }

        if ($node instanceof Node\Stmt\Use_) {
            // @var Node\Stmt\UseUse $user
            foreach ($node->uses as $use) {
                $alias = $use->alias;

                if ($use->type !== Node\Stmt\Use_::TYPE_CONSTANT) {
                    $this->imports[$alias] = (string)$use->name;
                }
            }
        }

        if ($node instanceof Node\Stmt\ClassLike) {
            $this->name = $node->name;

            return NodeTraverser::STOP_TRAVERSAL;
        }
    }
}
