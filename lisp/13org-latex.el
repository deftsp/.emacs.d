;;; 13org-latex.el ---                               -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Shihpin Tseng

;; Author: Shihpin Tseng <deftsp@gmail.com>
;; Keywords:

;; Links
;; [[https://emacs-china.org/t/topic/1577/10][org-mode 如何导出 pdf - Emacs China]]
;; [[https://pengpengxp.github.io/archive/before-2018-11-10/2018-05-30-org-mode-to-pdf.html][org-mode 写文档]]
;; [[https://github.com/tumashu/org2ctex][tumashu/org2ctex: Export org to ctex (a latex macro for Chinese)]]
;; [[https://github.com/CTeX-org/lshort-zh-cn][CTeX-org/lshort-zh-cn]]

(use-package ox-latex
  :defer t
  :init
  (setq org-latex-pdf-process
        '("xelatex -interaction nonstopmode -output-directory %o %f"
          "bibtex %b"
          "xelatex -interaction nonstopmode -output-directory %o %f"
          "xelatex -interaction nonstopmode -output-directory %o %f"))
  (setq org-latex-compiler "xelatex")
  ;; fontify source code
  (setq org-latex-listings t)           ; 'minted
  :config
  (add-to-list 'org-Latex-packages-alist '("" "listings"))
  ;; (add-to-list 'org-latex-packages-alist '("" "color"))
  ;; (add-to-list 'org-latex-packages-alist '("newfloat" "minted"))
  ;; (add-to-list 'org-latex-packages-alist '(("AUTO" "inputenc" t)))
  (add-to-list 'org-latex-packages-alist '("" "xcolor" t))
  (add-to-list 'org-latex-packages-alist '("" "listings" t))
  (add-to-list 'org-latex-packages-alist '("" "fontspec" t))
  (add-to-list 'org-latex-packages-alist '("" "indentfirst" t))
  (add-to-list 'org-latex-packages-alist '("" "xunicode" t))

  ;; https://github.com/tsdye/org-article
  ;; $ cp org-article.cls ~/Library/texmf/tex/latex/
  ;; $ kpsewhich org-article.cls
  (add-to-list 'org-latex-classes
               '("org-article" "\\documentclass{org-article}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]
"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  (add-to-list 'org-latex-classes
               '("ctexart" "\\documentclass[11pt]{ctexart}
[NO-DEFAULT-PACKAGES]
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{fixltx2e}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{wrapfig}
\\usepackage{rotating}
\\usepackage[normalem]{ulem}
\\usepackage{amsmath}
\\usepackage{textcomp}
\\usepackage{marvosym}
\\usepackage{wasysym}
\\usepackage{listings}
\\usepackage{amssymb}
\\usepackage{booktabs}
\\usepackage[colorlinks,linkcolor=black,anchorcolor=red,citecolor=black]{hyperref}
\\tolerance=1000

% 设置源码格式
\\lstset{framexleftmargin=5mm, frame=shadowbox, rulesepcolor=\\color{blue}}
\\lstset{basicstyle=\\tiny}
\\lstset{postbreak=\\space, breakindent=5pt, breaklines}

% 设置 verbatim 的字体大小
\\makeatletter
\\def\\verbatim{\\tiny\\@verbatim \\frenchspacing\\@vobeyspaces \\@xverbatim}
\\makeatother
"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  (setq org-latex-default-class "ctexart"))


(provide '13org-latex)
