<?php

require __DIR__ . '/0-bootstrap.inc.php';

$blocks = unserialize(file_get_contents('php://stdin'), ['allowed_classes' => false]);

# Read the existing syntax file with block markers in it.
#
$template = file_get_contents($argv[1]);

# Clean up any previously defined blocks.
$template = preg_replace(
    sprintf(
        '/(@block\([\'"](%s)[\'"]\)["\r\n\t ]*).*?(["\r\n\t ]*@endblock)\b/ism',
        implode(/* $glue = */ '|', array_keys($blocks))
    ),
    "\\1___\\2___\\3",
    $template
);

# Update block contents in the template.
foreach ($blocks as $blockName => $lines) {
    $template = str_ireplace(
        sprintf('___%s___', $blockName),
        rtrim(is_array($lines) ? implode(/* $glue = */ "\n", $lines) : $lines),
        $template
    );
}

$template = preg_replace_callback(
    '/
        (?<begin>"\s*@copy\s+(?<copy>[a-zA-Z0-9_]+)(?<processors>(\s+(strip_maximum_size))+)?)
        (?<script>.*?)
        (?<end>"\s*@end\s+([a-zA-Z0-9_]+))
    /sx',
    function (array $groups) use ($template) {
        $copy = preg_quote($groups['copy'], /* $delimiter = */ '/');

        $processors = array_filter(array_map('trim', preg_split('{[;, ]+}', $groups['processors'])));

        preg_match("/
            \"\\s*@begin\\s+{$copy}\b
            (?<script>.*?)
            \"\\s*@end\\s+{$copy}\b
        /sx", $template, $captures);

        if (! isset($captures['script'])) {
            file_put_contents('php://stderr', "[ERROR] The block referenced by '{$groups['begin']}' was not found." . PHP_EOL);

            return $groups['begin'] . $groups['script'] . $groups['end'];
        }

        $script = $captures['script'];

        foreach ($processors as $processor) {
            switch ($processor) {
            case 'strip_maximum_size':
                $script = preg_replace('{\\\\@\d+}', '\@', $script);
                break;

            default:
                file_put_contents('php://stderr', "[ERROR] The processor \"{$processor}\" is not supported, found in '{$groups['begin']}'." . PHP_EOL);
            }
        }

        return $groups['begin'] . $script . $groups['end'];
    },
    $template
);

echo $template;
