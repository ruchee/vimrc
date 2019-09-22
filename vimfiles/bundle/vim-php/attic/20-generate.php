<?php

require __DIR__ . '/0-bootstrap.inc.php';

$extensions = [];
$versions = [];

while (false !== $line = fgets(STDIN)) {
    $unserialized = unserialize($line, ['allowed_classes' => false]);
    foreach ($unserialized as $extension => &$collected) {
        if (! isset($extensions[$extension])) {
            $extensions[$extension] = $collected;
            continue;
        }

        $extensions[$extension] = array(
            'name' => $extensions[$extension]['name'],
            'versions' => array_merge($extensions[$extension]['versions'], $collected['versions']),
            'classes' => array_merge($extensions[$extension]['classes'], $collected['classes']),
            'functions' => array_merge($extensions[$extension]['functions'], $collected['functions']),
            'constants' => array_merge($extensions[$extension]['constants'], $collected['constants']),
        );
    }
}

foreach ($extensions as &$collected) {
    $collected['classes'] = array_unique($collected['classes']);
    $collected['functions'] = array_unique($collected['functions']);
    $collected['constants'] = array_unique($collected['constants']);

    sort($collected['classes'], SORT_NATURAL);
    sort($collected['functions'], SORT_NATURAL);
    sort($collected['constants'], SORT_NATURAL);

    $versions = array_merge($versions, $collected['versions']);
}

$versions = array_unique($versions);
sort($versions, SORT_NATURAL);

$blocks = array(
    'extensions' => array(),
    'last-modified' => sprintf(
        '%s, PHP %s',
        date('r' /* RFC 2822 */),
        implode(/* $glue = */ ', ', $versions)
    ),
);

$blocks['extensions'][] = 'if ! exists("g:php_syntax_extensions_enabled")';
$blocks['extensions'][] = sprintf('    let g:php_syntax_extensions_enabled = ["%s"]', implode(/* $glue = */ '", "', array_map('strtolower', array_keys($extensions))));
$blocks['extensions'][] = 'endif';

$blocks['extensions'][] = 'if ! exists("g:php_syntax_extensions_disabled")';
$blocks['extensions'][] = '    let g:php_syntax_extensions_disabled = []';
$blocks['extensions'][] = 'endif';

$ifExtensionEnabled = function ($extensionName) {
    return sprintf(
        'if ' .
        'index(g:php_syntax_extensions_enabled, "%1$s") >= 0 && ' .
        'index(g:php_syntax_extensions_disabled, "%1$s") < 0 && ' .
        '( ! exists("b:php_syntax_extensions_enabled") || index(b:php_syntax_extensions_enabled, "%1$s") >= 0) && ' .
        '( ! exists("b:php_syntax_extensions_disabled") || index(b:php_syntax_extensions_disabled, "%1$s") < 0)',
        strtolower($extensionName)
    );
};

$blocks['extensions'][] = 'syn case match';

foreach ($extensions as $extension) {
    if (! count($extension['constants'])) {
        continue;
    }

    $blocks['extensions'][] = $ifExtensionEnabled($extension['name']);
    $blocks['extensions'][] = sprintf('" %s constants', $extension['name']);
    $blocks['extensions'][] = sprintf('syn keyword phpConstants %s contained', implode(/* $glue = */ ' ', $extension['constants']));
    $blocks['extensions'][] = 'endif';
}

$blocks['extensions'][] = 'syn case ignore';

foreach ($extensions as $extension) {
    if (! count($extension['functions']) && ! count($extension['classes'])) {
        continue;
    }

    $blocks['extensions'][] = $ifExtensionEnabled($extension['name']);

    if (count($extension['functions'])) {
        $blocks['extensions'][] = sprintf('" %s functions', $extension['name']);
        $blocks['extensions'][] = sprintf('syn keyword phpFunctions %s contained', implode(/* $glue = */ ' ', $extension['functions']));
    }
    if (count($extension['classes'])) {
        $blocks['extensions'][] = sprintf('" %s classes and interfaces', $extension['name']);
        $blocks['extensions'][] = sprintf('syn keyword phpClasses %s contained', implode(/* $glue = */ ' ', $extension['classes']));
    }

    $blocks['extensions'][] = 'endif';
}

echo serialize($blocks);
