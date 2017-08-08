{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE CPP #-}

-- We can't provide all signatures anyway because CMark doesn't export
-- OnEnter and OnExit (which are needed to provide a signature for e.g.
-- CustomBlock)
#if __GLASGOW_HASKELL__ >= 800
{-# OPTIONS -fno-warn-missing-pattern-synonym-signatures #-}
#endif


-- |
-- <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/glasgow_exts.html#ghc-flag--XPatternSynonyms Pattern synonyms>
-- for working with the cmark AST. You can construct and deconstruct the AST
-- with them as if they were ordinary constructors.
--
-- Each pattern comes in two versions; if you don't care about position info,
-- use the one with the underscore. (If you try to use it for construction,
-- it will use 'Nothing' as position info.)
--
-- Since list items can only contain 'ITEM', we provide additional patterns
-- for lists – 'ListItems' and 'ListItems_' – which automatically unwrap them.
module CMark.Patterns
(
  -- ** 'DOCUMENT'
  pattern Document,
  pattern Document_,
  -- ** 'THEMATIC_BREAK' (has no children)
  pattern ThematicBreak,
  pattern ThematicBreak_,
  -- ** 'PARAGRAPH'
  pattern Paragraph,
  pattern Paragraph_,
  -- ** 'BLOCK_QUOTE'
  pattern BlockQuote,
  pattern BlockQuote_,
  -- ** 'HTML_BLOCK' (has no children)
  pattern HtmlBlock,
  pattern HtmlBlock_,
  -- ** 'CUSTOM_BLOCK'
  pattern CustomBlock,
  pattern CustomBlock_,
  -- ** 'CODE_BLOCK' (has no children)
  pattern CodeBlock,
  pattern CodeBlock_,
  -- ** 'HEADING'
  pattern Heading,
  pattern Heading_,
  -- ** 'LIST'
  pattern List,
  pattern List_,
  pattern ListItems,
  pattern ListItems_,
  -- ** 'ITEM'
  pattern Item,
  pattern Item_,
  -- ** 'TEXT' (has no children)
  pattern Text,
  pattern Text_,
  -- ** 'SOFTBREAK' (has no children)
  pattern Softbreak,
  pattern Softbreak_,
  -- ** 'LINEBREAK' (has no children)
  pattern Linebreak,
  pattern Linebreak_,
  -- ** 'HTML_INLINE' (has no children)
  pattern HtmlInline,
  pattern HtmlInline_,
  -- ** 'CUSTOM_INLINE'
  pattern CustomInline,
  pattern CustomInline_,
  -- ** 'CODE' (has no children)
  pattern Code,
  pattern Code_,
  -- ** 'EMPH'
  pattern Emph,
  pattern Emph_,
  -- ** 'STRONG'
  pattern Strong,
  pattern Strong_,
  -- ** 'LINK'
  pattern Link,
  pattern Link_,
  -- ** 'IMAGE'
  pattern Image,
  pattern Image_,
)
where


import CMark


----------------------------------------------------------------------------
-- Ordinary patterns
----------------------------------------------------------------------------

pattern Document pos xs = Node pos DOCUMENT xs

pattern ThematicBreak pos <- Node pos THEMATIC_BREAK _
  where ThematicBreak pos =  Node pos THEMATIC_BREAK []

pattern Paragraph pos xs = Node pos PARAGRAPH xs

pattern BlockQuote pos xs = Node pos BLOCK_QUOTE xs

pattern HtmlBlock pos t <- Node pos (HTML_BLOCK t) _
  where HtmlBlock pos t =  Node pos (HTML_BLOCK t) []

pattern CustomBlock pos en ex xs = Node pos (CUSTOM_BLOCK en ex) xs

pattern CodeBlock pos info t <- Node pos (CODE_BLOCK info t) _
  where CodeBlock pos info t =  Node pos (CODE_BLOCK info t) []

pattern Heading pos lvl xs = Node pos (HEADING lvl) xs

pattern List pos attrs xs = Node pos (LIST attrs) xs

pattern Item pos xs = Node pos ITEM xs

pattern Text pos t <- Node pos (TEXT t) _
  where Text pos t =  Node pos (TEXT t) []

pattern Softbreak pos <- Node pos SOFTBREAK _
  where Softbreak pos =  Node pos SOFTBREAK []

pattern Linebreak pos <- Node pos LINEBREAK _
  where Linebreak pos =  Node pos LINEBREAK []

pattern HtmlInline pos t <- Node pos (HTML_INLINE t) _
  where HtmlInline pos t =  Node pos (HTML_INLINE t) []

pattern CustomInline pos en ex xs = Node pos (CUSTOM_INLINE en ex) xs

pattern Code pos t <- Node pos (CODE t) _
  where Code pos t =  Node pos (CODE t) []

pattern Emph pos xs = Node pos EMPH xs

pattern Strong pos xs = Node pos STRONG xs

pattern Link pos url title xs = Node pos (LINK url title) xs

pattern Image pos url title xs = Node pos (IMAGE url title) xs

----------------------------------------------------------------------------
-- Patterns without position info
----------------------------------------------------------------------------

pattern Document_ xs <- Document _       xs
  where Document_ xs =  Document Nothing xs

pattern ThematicBreak_ <- ThematicBreak _      
  where ThematicBreak_ =  ThematicBreak Nothing

pattern Paragraph_ xs <- Paragraph _       xs
  where Paragraph_ xs =  Paragraph Nothing xs

pattern BlockQuote_ xs <- BlockQuote _       xs
  where BlockQuote_ xs =  BlockQuote Nothing xs

pattern HtmlBlock_ t <- HtmlBlock _       t
  where HtmlBlock_ t =  HtmlBlock Nothing t

pattern CustomBlock_ en ex xs <- CustomBlock _       en ex xs
  where CustomBlock_ en ex xs =  CustomBlock Nothing en ex xs

pattern CodeBlock_ info t <- CodeBlock _       info t
  where CodeBlock_ info t =  CodeBlock Nothing info t

pattern Heading_ lvl xs <- Heading _       lvl xs
  where Heading_ lvl xs =  Heading Nothing lvl xs

pattern List_ attrs xs <- List _       attrs xs
  where List_ attrs xs =  List Nothing attrs xs

pattern Item_ xs <- Item _       xs
  where Item_ xs =  Item Nothing xs

pattern Text_ t <- Text _       t
  where Text_ t =  Text Nothing t

pattern Softbreak_ <- Softbreak _
  where Softbreak_ =  Softbreak Nothing

pattern Linebreak_ <- Linebreak _
  where Linebreak_ =  Linebreak Nothing

pattern HtmlInline_ t <- HtmlInline _       t
  where HtmlInline_ t =  HtmlInline Nothing t

pattern CustomInline_ en ex xs <- CustomInline _       en ex xs
  where CustomInline_ en ex xs =  CustomInline Nothing en ex xs

pattern Code_ t <- Code _       t
  where Code_ t =  Code Nothing t

pattern Emph_ xs <- Emph _       xs
  where Emph_ xs =  Emph Nothing xs

pattern Strong_ xs <- Strong _       xs
  where Strong_ xs =  Strong Nothing xs

pattern Link_ url title xs <- Link _       url title xs
  where Link_ url title xs =  Link Nothing url title xs

pattern Image_ url title xs <- Image _       url title xs
  where Image_ url title xs =  Image Nothing url title xs

----------------------------------------------------------------------------
-- ListItems
----------------------------------------------------------------------------

unwrapItems :: [Node] -> [(Maybe PosInfo, [Node])]
unwrapItems is = [(pos, xs) | Item pos xs <- is]

unwrapItems_ :: [Node] -> [[Node]]
unwrapItems_ is = [xs | Item_ xs <- is]

pattern ListItems pos attrs xs <- Node pos (LIST attrs) (unwrapItems -> xs)
  where ListItems pos attrs xs =  Node pos (LIST attrs) (map (uncurry Item) xs)

pattern ListItems_ attrs xs <- Node _       (LIST attrs) (unwrapItems_ -> xs)
  where ListItems_ attrs xs =  Node Nothing (LIST attrs) (map Item_ xs)
