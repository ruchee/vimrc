au BufNewFile,BufRead *.mm,*.mmd,*.mermaid set filetype=mermaid |
      \ setlocal indentexpr=MermaidIndent() |
      \ setlocal indentkeys+==end,=deactivate,=classDiagram,=classDiagram-v2,=erDiagram |
      \ setlocal indentkeys+==gantt,=graph,=flowchart,=pie,=sequenceDiagram,=stateDiagram,=stateDiagram-v2
