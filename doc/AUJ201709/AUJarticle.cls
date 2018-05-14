%%
%% This is file `AUJarticle.cls',
%%
%% LaTeX article class for Ada User Journal
%%
%% (C) Kristoffer Nyborg Gregertsen 2011
%% Adapted by Luis Miguel Pinho (2012)
%% Experimental 

\NeedsTeXFormat{LaTeX2e}

\ProvidesClass{AUJarticle}[2011/11/10 Class for Ada User Journal articles]%


%% Counter For different institutions
\newcounter{auths}
\setcounter{auths}{0}

%% Update the counter and print it
\newcommand{\foo}{%
        \stepcounter{auths}%
        \theauths}

%% Define the accumulator for the authors 
\newcommand{\authorsd}{}
%% Add a new author to the accumulator. Currently adds a number
%% indexing each set of authors, by the institution where they belong to
\newcommand{\addauthor}[3]{\makeatletter\g@addto@macro\authorsd{{\bf {\em #1}}\par{#2};\;email:\;{#3}\par}\makeatother}
% Shortcut for the resulting list of authors. To be used in the
% creation of the author list in the titlepage
\newcommand{\Authors}{\authorsd}

%\newcommand{\Affiliation}{The Ada Research Institute, Lovelace Road,
%  Oxbridge OB5 1AZ.; Tel: +44 123 987 6543; email: ana@ada.org}

\newcommand{\ShortAuthor}{}
\newcommand{\ShortTitle}{}

%% This New Command here checks If the title Is too Long To be On the headings. If so, prints the short title instead, If Not keeps the original title
\newsavebox{\@justcentbox}
\newcommand{\titleorshort}{%
  \sbox \@justcentbox{\@title}% 
  \ifdim \wd \@justcentbox >21pc \ifthenelse{\equal{\ShortTitle}{}}{Define short title plese}{\ShortTitle}%
  \else \@title \fi
  }

%% These New commands define the volume, the number, And the Month And Year of the publication
%% If the proper information Is To be presented, the values of these commands must be initialized
%% using the adequate user level commands of the package defined bellow
\newcommand{\IVolume}{}
\newcommand{\INumber}{}
\newcommand{\IDate}{}
\newcommand{\Issue}{
\ifthenelse{\equal{\IVolume}{}\TE@or\equal{\INumber}{}\TE@or\equal{\IDate}{}}
 {Proper information about publication indexing missing!!!}{Volume \IVolume, Number \INumber, \datef\IDate}}



%% Use article class as base, 10pt fonts on a4 paper
\LoadClass[a4paper,10pt,twocolumn,twoside]{article}

%% Use times font and adjust some default font sizes
\RequirePackage[T1]{fontenc}
\RequirePackage{times}
\RequirePackage[utf8x]{inputenc}
\RequirePackage{datetime}
\RequirePackage{xspace}

%% Definition of the Date Format that Is the one adopted by this journal

\newdateformat{datef}{\monthname[\THEMONTH] \THEYEAR}

\renewcommand{\large}{\fontsize{11pt}{12.2pt}}
\renewcommand{\Large}{\fontsize{13pt}{14.6pt}}

%% Customize margins, paragraphs and Columns

\RequirePackage[margin=3.8pc,top=68pt,bottom=68pt]{geometry}
\RequirePackage{parskip}

\setlength{\columnsep}{2pc}
\setlength{\parskip}{6pt plus 6pt minus 3pt}

%% Customize headers and footers, requires letter spacing

\RequirePackage{fancyhdr}
\RequirePackage{microtype}

\newcommand{\texthf}[1]{\fontsize{10pt}{12pt}\textsf{\textls[115]{#1}}}

\fancypagestyle{plain}{%
  \fancyhead[RE,LO]{}%
}

\AtBeginDocument{
  \pagestyle{fancy}
  \renewcommand{\headrulewidth}{0pt}
  \renewcommand{\footrulewidth}{1pt}
  \fancyhead[RE]{\texthf{\titleorshort}}
  \fancyhead[LO]{\texthf{\ShortAuthor}}
  \fancyhead[LE,RO]{\textbf{\texthf{\thepage}}}
  \fancyfoot[LE,RO]{\texthf{\Issue}}
  \fancyfoot[RE,LO]{\texthf{Ada User Journal}}
  \fancyfootoffset{1pt}
  \fancyfoot[C]{}
}

%% Customize captions
\usepackage[small,bf]{caption}
\renewcommand{\captionfont}{\small\bfseries}

%% Customize section headings
\usepackage{titlesec}

\titlespacing*{\section}{0pt}{6pt plus 6pt minus 3pt}{0pt plus 3pt minus 1pt}
\titlespacing*{\subsection}{0pt}{3pt plus 6pt minus 1pt}{0pt plus 3pt minus 1pt}
\titlespacing*{\subsubsection}{0pt}{3pt plus 6pt minus 1pt}{0pt plus 3pt minus 1pt}

\titleformat{\section}{\Large\bfseries}{\thesection}{1em}{}
\titleformat{\subsection}{\large\bfseries}{\thesubsection}{1em}{}
\titleformat{\subsubsection}{\large\it}{\thesubsubsection}{1em}{}

%% Customize abstract
\RequirePackage{abstract}

\renewcommand{\absnamepos}{flushleft}
\renewcommand{\abstitleskip}{6pt plus 3pt minus 0pt}
\renewcommand{\absleftindent}{0pt}
\renewcommand{\absrightindent}{0pt}
\renewcommand{\abstractnamefont}{\Large\bf}
\renewcommand{\abstracttextfont}{\large\it}

%% Setup listing environment for Ada
\RequirePackage{listings}

%% TODO: \lstnewenvironment{ada}[1][2005]{
\lstset{
  language=[2005]Ada,
  xleftmargin=1pc,
  xrightmargin=1pc,
  commentstyle=\textit,
  basicstyle=\footnotesize\sffamily,
  columns=flexible,
  captionpos=t,
  abovecaptionskip=-10pt,
  belowcaptionskip=10.5pt,
  %%frame=lines,
  framerule=0.8pt,
  framesep=3pt,
  numbers=none
}
%%{}

\newcommand{\ada}{\lstinline[basicstyle=\footnotesize]}


%% Redifinition of volume And issue : these are the user level commands that *MUST* be used in order To define
%% a proper running info about the edition of the journal

\newcommand{\issuev}[1]{\renewcommand{\IVolume}{#1}}
\newcommand{\issuen}[1]{\renewcommand{\INumber}{#1}}
\newcommand{\issued}[1]{\renewcommand{\IDate}{#1}}
\newcommand{\shorttitle}[1]{\renewcommand{\ShortTitle}{#1}}


%% Define commands to overwrite default values

\newcommand{\affiliation}[1]{\renewcommand{\Affiliation}{#1}}
\newcommand{\shortauthor}[1]{\renewcommand{\ShortAuthor}{#1}}


%% Title on top of first page, start two-columns after this
\renewcommand\maketitle{\par
  \begingroup
    \renewcommand\thefootnote{\@fnsymbol\c@footnote}%
    \def\@makefnmark{\rlap{\@textsuperscript{\normalfont\@thefnmark}}}%
    \long\def\@makefntext##1{\parindent 1em\noindent
      \hb@xt@1.8em{%
        \hss\@textsuperscript{\normalfont\@thefnmark}}##1}%
    \if@twocolumn
      \ifnum \col@number=\@ne
        \@AUJtitle
      \else
        \twocolumn[\@AUJtitle]%
      \fi
    \fi
    \thispagestyle{plain}\@thanks
    \endgroup
  \setcounter{footnote}{0}%
}

\def\@AUJtitle{%
  \newpage
  \null
  \raggedright
  {\Huge \@title \par}%
  \vskip 12pt%
  {\fontsize{12pt}{13.2pt} \textbf{\textit{\@author}} \par}%
  \vskip 3pt%
  {\large \textit{\Authors} \par}%
  \vskip 2pc}

%% End of file `AUJarticle.cls'.
