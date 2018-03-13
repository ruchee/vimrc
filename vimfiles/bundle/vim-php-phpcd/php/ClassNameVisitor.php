<?php
namespace PHPCD;

use PhpParser\Node;
use PhpParser\NodeVisitorAbstract;
use PhpParser\NodeTraverser;

class ClassNameVisitor extends NodeVisitorAbstract
{
    public $name;

    public function leaveNode(Node $node)
    {
        if ($node instanceof Node\Stmt\Class_ && isset($node->namespacedName)) {
            $this->class = (string)$node->namespacedName;
            return NodeTraverser::STOP_TRAVERSAL;
        }
    }
}
