<?php
class Test {

    private static function a($a) {
        if ($a) {
            return 1;
        }
        /**/
        ++$a;return ;//
    }

    private static function b($a) {
        if ($a) {
            return;
        }

        return null;
    }
}
