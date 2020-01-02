module IO.Markdown
    ( parse
    , serialize
    , MarkdownInfo(MarkdownInfo)
    ) where

import IO.Markdown.Parser     (parse)
import IO.Markdown.Serializer (MarkdownInfo (MarkdownInfo), serialize)
