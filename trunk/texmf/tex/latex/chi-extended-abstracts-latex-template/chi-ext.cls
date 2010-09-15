% CHI Extended Abstracts template.
% Tested with XeTeX on Mac OS X (Get it from http://tug.org/mactex)
% The latest version is available at <http://manas.tungare.name/software/latex/>
% 
% Filename: chi-ext.cls
% 
% CHANGELOG:
%   2010-08-09   Manas Tungare      Updated copyright info for CHI 2011
%   2009-12-04   Stephen Voida      Updated copyright info for CHI 2010
%   2008-11-25   Manas Tungare      Initial create.
%   2009-11-17   Manas Tungare      Refactored the title & author sections.
% 
% LICENSE:
%   Public domain: You are free to do whatever you want with this template.
%   If you improve this in any way, please drop me a note <manas@tungare.name>,
%   so I can share the updates with everyone.
%   
%   PLEASE RECONSIDER BEFORE FORKING THIS TEMPLATE; there are already
%   several versions of the chiproceedings template for no good reason.
%   DO NOT REDISTRIBUTE THIS FILE UNDER A DIFFERENT FILENAME unless you
%   have a very good reason to change its name.

\NeedsTeXFormat{LaTeX2e}[1995/12/01]
\ProvidesClass{chi-ext}[2008/11/25, Manas Tungare, manas@tungare.name]

\typeout{CHI Extended Abstracts template, by Manas Tungare.}
\typeout{Available from http://manas.tungare.name/software/latex/}

\LoadClass[9pt,letterpaper,landscape]{article}[1996/10/31]

\RequirePackage{graphicx}
\RequirePackage{graphics}
\RequirePackage{color}
\RequirePackage{multicol}
\RequirePackage{float}
% \RequirePackage[landscape]{geometry}

% ---------------------
% Define our variables

\newcommand*{\copyrightinfo}[1]{\gdef\@copyrightinfo{#1}}
\newcommand*{\keywords}[1]{\gdef\@keywords{#1}}
\newcommand*{\acmclassification}[1]{\gdef\@acmclassification{#1}}

% ----------------------------------------------------
% Typeset titles of chapters & sections in sans-serif

\def\@makechapterhead#1{%
  \vspace*{50\p@}%
  {\parindent \z@ \raggedright \sffamily
    \ifnum \c@secnumdepth >\m@ne
        \large \bfseries \@chapapp\space \thechapter
        \par\nobreak
        \vskip 10\p@
    \fi
    \interlinepenalty\@M
    \Huge \bfseries #1\par\nobreak
    \vskip 20\p@
  }}

\def\@makeschapterhead#1{%
  \vspace*{50\p@}%
  {\parindent \z@ \raggedright
    \sffamily
    \interlinepenalty\@M
    \Huge\bfseries  #1\par\nobreak
    \vskip 20\p@
  }}

\renewcommand\section{\@startsection {section}{1}{\z@}%
                                   {-3.5ex \@plus -0.1ex \@minus -.2ex}%
                                   {0.2ex 
                                   % \@plus.01ex
                                   }%
                                   {\sffamily\large\bfseries}}
\renewcommand\subsection{\@startsection{subsection}{2}{\z@}%
                                   {-3.25ex\@plus -0.1ex \@minus -.2ex}%
                                   {0.2ex \@plus .01ex}%
                                   {\sffamily\normalsize\bfseries}}
\renewcommand\subsubsection{\@startsection{subsubsection}{3}{\z@}%
                                   {-3.25ex\@plus -0.1ex \@minus -.2ex}%
                                   {0.2ex \@plus .01ex}%
                                   {\sffamily\MakeUppercase\small\bfseries}}

% ----------------------------------------
% No numbering for any {sub{sub}}sections

\setcounter{secnumdepth}{-1}

% -------------------
% Use OpenType fonts

\RequirePackage[cm-default]{fontspec}
\RequirePackage{xunicode}
\RequirePackage{xltxtra}

\defaultfontfeatures{Scale=MatchLowercase}

\setmainfont[Mapping=tex-text,Scale=0.8]{Verdana}
\setsansfont[Mapping=tex-text,Scale=0.9]{Verdana}
\setmonofont{Monaco}

% ----------------------------------------------
% Hyperref for fancy PDF metadata and bookmarks

\RequirePackage[
  colorlinks,
  citecolor=blue,
  linkcolor=blue,
  menucolor=blue,
  urlcolor=blue,
  linktocpage,
  naturalnames,
  plainpages,
  final
]{hyperref}

% -------------------------
% Set paper & margin sizes

\setlength{\paperheight}{8.5in}
\setlength{\paperwidth}{11in}

\setlength{\textwidth}{7.25in}
\setlength{\textheight}{6in}

\setlength{\oddsidemargin}{1.5in}
\setlength{\topmargin}{0.25in}

\setlength{\columnsep}{.5in}

\setlength{\voffset}{-0.2in}

\setlength{\parindent}{0pt}
\setlength{\footskip}{0.5in}

\setlength{\itemsep}{0pt}
\setlength{\parsep}{0pt}
\setlength{\topsep}{0pt}

\setlength{\parskip}{0pt}
% \addtolength{\parskip}{0.25\baselineskip}

\pagestyle{empty}  % No page numbers.

\clubpenalty=10000
\widowpenalty=10000

% ---------------------------------------------------------
% Replace the original title-making code with our renderer

\renewcommand{\maketitle}{%
  \begin{tabular}{p{0.15\textwidth}p{0.85\textwidth}}
    & 
    {
      \rule{0.8\textwidth}{3pt}
      \setlength{\parskip}{3pt}
      \fontsize{30}{30}
      \fontspec{Helvetica}
      \bfseries

      \vspace{-0.5mm}

      \addfontfeature{LetterSpace=-4.17}{
        \makeatletter  \@title \makeatother
      }
    }
    \\
    \vspace{0.2in}
  \end{tabular}
}

\newcommand{\makeauthors}{
  \makeatletter \@author \makeatother
}

\newcommand{\makecopyright}{
  \vfil\null
  \rule{\columnwidth}{0.5pt}
    \makeatletter \@copyrightinfo \makeatother
  }

\endinput