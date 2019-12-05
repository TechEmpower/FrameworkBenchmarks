<?php

namespace DreamCat\Benchmark\View;

use DreamCat\Benchmark\Entry\Fortune\FortuneEntry;

/**
 * Fortune的视图层
 * @author vijay
 */
class FortuneView
{
    /**
     * 生成message的html
     * @param FortuneEntry[] $list
     * @return string
     */
    public function messageHtml(array $list): string
    {
        $html = implode("", array_map(function (FortuneEntry $entry) {
            $msg = htmlspecialchars($entry->getMessage(), ENT_QUOTES, "UTF-8");
            return "<tr><td>{$entry->getId()}</td><td>{$msg}</td></tr>";
        }, $list));
        return "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th>"
            . "<th>message</th></tr>{$html}</table></body></html>";
    }
}

# end of file
