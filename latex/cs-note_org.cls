\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{cs-note_org}[2025/01/01 CS_ORG Org Template]

% =====================
% 基础文档类
% =====================
\LoadClass[12pt]{article}

% =====================
% 页面与中文
% =====================
\usepackage[margin=2.5cm]{geometry}
\usepackage{ctex}

% =====================
% 数学支持
% =====================
\usepackage{amsmath, amssymb, amsfonts}  % 基础数学包
\usepackage{bm}                           % 粗体数学符号
\usepackage{mathtools}                    % amsmath 的增强

% =====================
% 图形与颜色
% =====================
\usepackage{tikz}
\usepackage{xcolor}
\usepackage{graphicx}
\usepackage{float}
\usepackage{caption}

% =====================
% 超链接（Org 导出需要）
% =====================
\usepackage{hyperref}
\hypersetup{
    colorlinks=true,
    linkcolor=blue,
    filecolor=magenta,      
    urlcolor=cyan,
}

% =====================
% 超美代码框颜色
% =====================
\definecolor{boxblue}{RGB}{70, 130, 180}
\definecolor{boxgreen}{RGB}{34, 139, 34}
\definecolor{boxpurple}{RGB}{148, 0, 211}
\definecolor{boxgray}{RGB}{108, 117, 125}
\definecolor{boxback}{RGB}{252, 252, 252}
\definecolor{boxframe}{RGB}{220, 220, 220}

% =====================
% Minted（代码高亮）
% =====================
\usepackage{minted}
\setminted{
  breaklines=true,
  breakanywhere=true,
  frame=leftline,
  framerule=2pt,
  rulecolor=\color{boxblue},
  bgcolor=boxback,
  fontsize=\small,
  baselinestretch=1.2
}

% 为 Org mode 导出设置默认语言环境
\usemintedstyle{default}

% =====================
% Tcolorbox
% =====================
\usepackage{tcolorbox}
\tcbuselibrary{listings,skins,breakable}

% =====================
% 定义注意事项框
% =====================
\newtcolorbox{NOTE}{
    enhanced,
    breakable,
    colback=blue!5!white,
    colframe=blue!50!black,
    arc=3pt,
    boxrule=0.5pt,
    left=8pt,
    right=8pt,
    title={\textbf{注意}},
    fonttitle=\sffamily\bfseries,
    coltitle=blue!80!black,
    colbacktitle=blue!20!white
}

% =====================
% 定义警告框
% =====================
\newtcolorbox{WARNING}{
    enhanced,
    breakable,
    colback=red!5!white,
    colframe=red!50!black,
    arc=3pt,
    boxrule=0.5pt,
    left=8pt,
    right=8pt,
    title={\textbf{警告}},
    fonttitle=\sffamily\bfseries,
    coltitle=red!80!black,
    colbacktitle=red!20!white
}

% =====================
% 定义提示框
% =====================
\newtcolorbox{TIP}{
    enhanced,
    breakable,
    colback=green!5!white,
    colframe=green!50!black,
    arc=3pt,
    boxrule=0.5pt,
    left=8pt,
    right=8pt,
    title={\textbf{提示}},
    fonttitle=\sffamily\bfseries,
    coltitle=green!80!black,
    colbacktitle=green!20!white
}

% =====================
% 定义示例框
% =====================
\newtcolorbox{EXAMPLE}{
    enhanced,
    breakable,
    colback=boxpurple!5!white,
    colframe=boxpurple!60!black,
    arc=3pt,
    boxrule=0.5pt,
    left=8pt,
    right=8pt,
    title={\textbf{示例}},
    fonttitle=\sffamily\bfseries,
    coltitle=boxpurple!90!black,
    colbacktitle=boxpurple!20!white
}


\newtcolorbox{ep}[1][]{%
    enhanced,
    colback=JungleGreen!10!Cerulean!15,
    colframe=CornflowerBlue!60!Black,
    title=\textbf{Example},
    #1
}

% =====================
% 章节深度（必须在 titlesec 之前）
% =====================
\setcounter{secnumdepth}{3}
\setcounter{tocdepth}{2}

% =====================
% 章节标题样式（titlesec 必须在设置编号格式之后加载）
% =====================
\usepackage{titlesec}

% =====================
% 强制使用阿拉伯数字编号（必须在 titlesec 之后）
% =====================
\renewcommand{\thesection}{\arabic{section}}
\renewcommand{\thesubsection}{\thesection.\arabic{subsection}}
\renewcommand{\thesubsubsection}{\thesubsection.\arabic{subsubsection}}

% 重新定义章节格式，确保使用数字
\titleformat{\section}
  {\normalfont\Large\bfseries\color{blue!70!black}}
  {\thesection}{1em}{}
  
\titleformat{\subsection}
  {\normalfont\large\bfseries\color{blue!60!black}}
  {\thesubsection}{1em}{}
  
\titleformat{\subsubsection}
  {\normalfont\normalsize\bfseries\color{blue!50!black}}
  {\thesubsubsection}{1em}{}

\endinput
