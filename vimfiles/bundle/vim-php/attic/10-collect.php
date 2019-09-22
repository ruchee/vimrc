<?php
/**
 * Script to update Vim's PHP syntax file.
 *
 * @author Stan Angeloff <stanimir@angeloff.name>
 *
 * @author Paul Garvin <paul@paulgarvin.net>
 * @copyright Copyright 2009 Paul Garvin
 * @license http://www.opensource.org/licenses/mit-license.php MIT License
 *
 * Loosely based on Paul Garvin <paul@paulgarvin.net> original script.
 *
 * For the full copyright and license information,
 * please view the LICENSE file that was distributed with this source code.
 */

require __DIR__ . '/0-bootstrap.inc.php';

# Parse the configuration file associated with this script.
$configuration = parse_ini_file(__DIR__ . '/syntax.ini', /* $process_sections = */ true);

# Process extensions and serialize built-in functions, constants, classes and interfaces.
$extensions = array();

foreach ($configuration['extensions'] as $extensionName => $isEnabled) {
    if (! $isEnabled) {
        continue;
    }

    try {
        $reflect = new \ReflectionExtension($extensionName);

        $collected = array(
            'name' => $reflect->getName(),
            'versions' => array(sprintf('%d.%d.%d', PHP_MAJOR_VERSION, PHP_MINOR_VERSION, PHP_RELEASE_VERSION)),
            'classes' => array(),
            'functions' => array_keys($reflect->getFunctions()),
            'constants' => array_diff(array_keys($reflect->getConstants()), array('TRUE', 'FALSE', 'NULL')),
        );

        foreach ($reflect->getClasses() as $extensionClass) {
            $collected['classes'][] = $extensionClass->getName();
            $collected['constants'] = array_unique(array_merge($collected['constants'], array_keys($extensionClass->getConstants())));
        }

        $extensions[$extensionName] = $collected;
    } catch (ReflectionException $e) {
        file_put_contents('php://stderr', sprintf('[ERROR] [PHP %d.%d] %\'.12s: %s.' . PHP_EOL, PHP_MAJOR_VERSION, PHP_MINOR_VERSION, $extensionName, rtrim($e->getMessage(), ' ?!.')));
    }
}

echo serialize($extensions) . PHP_EOL;
