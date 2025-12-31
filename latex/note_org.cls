%% notepaper.cls
%% 用于笔记和论文的 LaTeX 文档类
%% 基于 article 类，包含中文支持和彩色定理环境

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{note_org}[2024/12/30 v1.0 Note Paper Class with Chinese Support]

% 定义选项
\DeclareOption{UTF8}{\PassOptionsToPackage{UTF8}{ctex}}
\DeclareOption{fontset}{\PassOptionsToPackage{fontset=fandol}{ctex}}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{article}}
\ProcessOptions\relax

% 基于 article 类
\LoadClass{article}

% 页面设置
\RequirePackage[margin=2.5cm]{geometry}

% 中文支持（带选项）
\RequirePackage[UTF8,fontset=fandol]{ctex}

% hyperref 包（必须在大多数包之前加载）
\RequirePackage{hyperref}

% 数学相关包
\RequirePackage{mathrsfs}       % 提供 \mathscr
\RequirePackage{amsmath, amssymb}  % 基本数学符号
\RequirePackage{amsthm}
\RequirePackage{mathtools}

% TikZ 绘图
\RequirePackage{tikz}
\usetikzlibrary{arrows,positioning,shapes,calc}
\usetikzlibrary{arrows.meta}

% 颜色和彩色盒子（注意加载顺序）
\RequirePackage[dvipsnames]{xcolor}
\RequirePackage[most]{tcolorbox}

% 代码高亮
\RequirePackage{minted}
\setminted{breaklines=true,breakanywhere=true}

% 定义各种定理环境的漂亮配色
\newtcolorbox{definition}[1][]{%
    enhanced,
    colback=Emerald!10,
    colframe=cyan!40!black,
    title=\textbf{Definition},
    #1
}

\newtcolorbox{theorem}[1][]{%
    enhanced,
    colback=red!5,
    colframe=red!75!black,
    title=\textbf{Theorem},
    #1
}

\newtcolorbox{lemma}[1][]{%
    enhanced,
    colback=SeaGreen!10!CornflowerBlue!10,
    colframe=RoyalPurple!55!Aquamarine!100,
    title=\textbf{Lemma},
    #1
}

\newtcolorbox{proposition}[1][]{%
    enhanced,
    colback=Salmon!20,
    colframe=Salmon!90!Black,
    title=\textbf{Proposition},
    #1
}

\newtcolorbox{corollary}[1][]{%
    enhanced,
    colback=OliveGreen!10,
    colframe=Green!70,
    title=\textbf{\textcolor{white}{Corollary}},
    #1
}

\newtcolorbox{ep}[1][]{%
    enhanced,
    colback=JungleGreen!10!Cerulean!15,
    colframe=CornflowerBlue!60!Black,
    title=\textbf{Example},
    #1
}

\newtcolorbox{note}[1][]{%
    enhanced,
    colback=blue!5,
    colframe=blue!40,
    title=\textbf{Note},
    fonttitle=\small\bfseries,
    coltitle=blue!70!black,
    boxrule=1pt,
    arc=2pt,
    left=4mm,
    right=4mm,
    top=2mm,
    bottom=2mm,
    #1
}

\endinput
