# Emacs 配置插件清单

本文档详细列出了当前 Emacs 配置中安装的所有插件及其功能说明。

## 目录

- [包管理器](#包管理器)
- [Evil 模式及相关](#evil-模式及相关)
- [主题与图标](#主题与图标)
- [补全与导航](#补全与导航)
- [UI 增强](#ui-增强)
- [文件管理](#文件管理)
- [编程支持](#编程支持)
- [LSP 与语法检查](#lsp-与语法检查)
- [代码格式化](#代码格式化)
- [Snippet 系统](#snippet-系统)
- [Org 模式](#org-模式)
- [LaTeX 支持](#latex-支持)
- [PDF 查看](#pdf-查看)
- [启动页](#启动页)
- [版本控制](#版本控制)
- [输入法](#输入法)
- [Undo 系统](#undo-系统)
- [AI 辅助](#ai-辅助)
- [内置包配置](#内置包配置)
- [自定义模块](#自定义模块)

---

## 包管理器

### straight.el
- **功能**: 声明式包管理器，支持从 Git 仓库直接安装
- **配置位置**: `init-packages.el`

### use-package
- **功能**: 包配置宏，提供优雅的包管理语法
- **配置位置**: `init-packages.el`

---

## Evil 模式及相关

### evil
- **功能**: Vim 编辑器模拟，提供 Vim 风格的模态编辑
- **配置位置**: `init-packages.el`, `init-better-defaults.el`
- **快捷键**: 完整的 Vim 模式支持

### evil-surround
- **功能**: 快速添加/删除/修改括号、引号等包围符号
- **配置位置**: `init-packages.el`
- **快捷键**:
  - `gsa` - 添加包围
  - `gsd` - 删除包围
  - `gsr` - 替换包围

### evil-collection
- **功能**: 为各种 Emacs 模式提供 Evil 按键绑定
- **配置位置**: `init-packages.el`, `init-better-defaults.el`

---

## 主题与图标

### doom-themes
- **功能**: Doom Emacs 主题集合
- **配置位置**: `init-packages.el`
- **当前主题**: doom-one

### modus-themes
- **功能**: 高对比度、符合 WCAG AAA 标准的主题
- **配置位置**: `init-ui.el`, `init-keybindings.el`
- **变体**:
  - `modus-operandi-tinted` (浅色)
  - `modus-vivendi-tinted` (深色)

### nerd-icons
- **功能**: 提供 Nerd Fonts 图标支持
- **配置位置**: `init-packages.el`, `init-lsp.el`

### nerd-icons-completion
- **功能**: 为补全界面添加图标
- **配置位置**: `init-packages.el`

---

## 补全与导航

### vertico
- **功能**: 垂直补全界面，替代 Ivy/Helm
- **配置位置**: `init-packages.el`
- **设置**:
  - 显示 13 个候选项
  - 支持调整大小

### marginalia
- **功能**: 为补全候选项添加丰富的注释信息
- **配置位置**: `init-packages.el`

### orderless
- **功能**: 灵活的模糊匹配补全样式
- **配置位置**: `init-packages.el`

### embark
- **功能**: 上下文感知的操作菜单
- **配置位置**: `init-packages.el`, `init-keybindings.el`
- **快捷键**:
  - `C-.` / `C-;` - 执行操作
  - `C-c C-e` - 导出结果

### embark-consult
- **功能**: Embark 与 Consult 的集成
- **配置位置**: `init-packages.el`

### consult
- **功能**: 搜索和导航命令集合
- **配置位置**: `init-packages.el`, `init-keybindings.el`
- **主要功能**:
  - 缓冲区切换
  - 文件搜索
  - Ripgrep 集成
  - Imenu 导航

### consult-yasnippet
- **功能**: Consult 与 YASnippet 集成
- **配置位置**: `init-packages.el`
- **快捷键**: `SPC sn` - 搜索 snippet

### consult-todo
- **功能**: 在项目中搜索 TODO 注释
- **配置位置**: `init-packages.el`, `init-keybindings.el`
- **快捷键**: `SPC st` - 搜索 TODO

### wgrep
- **功能**: 可编辑的 grep 缓冲区
- **配置位置**: `init-packages.el`
- **功能**: 在搜索结果中直接编辑并保存

---

## UI 增强

### centaur-tabs
- **功能**: 标签栏，类似浏览器标签
- **配置位置**: `init-packages.el`
- **快捷键**:
  - `H` - 前一个标签
  - `L` - 后一个标签
- **特性**:
  - 修改状态指示器
  - 自动分组
  - 自定义配色

### highlight-indent-guides
- **功能**: 显示缩进参考线
- **配置位置**: `init-ui.el`
- **特性**:
  - 霓虹蓝配色
  - 当前层级高亮
  - 响应式显示

### keycast
- **功能**: 在模式行显示按键操作
- **配置位置**: `init-packages.el`
- **用途**: 演示和教学

### doom-modeline
- **功能**: 现代化的模式行
- **配置位置**: `init-packages.el`
- **特性**:
  - 显示 Git 状态
  - LSP 状态
  - 项目信息

### which-key
- **功能**: 显示可用的按键绑定提示
- **配置位置**: `init-code.el`, `init-keybindings.el`
- **设置**: 无延迟显示

---

## 文件管理

### treemacs
- **功能**: 项目文件树侧边栏
- **配置位置**: `init-ui.el`
- **快捷键**: `M-0` - 切换到 treemacs
- **特性**:
  - Git 集成
  - 文件监控
  - 项目管理

### treemacs-nerd-icons
- **功能**: Treemacs 的 Nerd Icons 主题
- **配置位置**: `init-ui.el`

### treemacs-evil
- **功能**: Treemacs 的 Evil 模式支持
- **配置位置**: `init-ui.el`
- **快捷键**: Vim 风格导航

### treemacs-projectile
- **功能**: Treemacs 与 Projectile 集成
- **配置位置**: `init-ui.el`

### treemacs-magit
- **功能**: Treemacs 与 Magit 集成
- **配置位置**: `init-ui.el`

### treemacs-perspective
- **功能**: Treemacs 与 Perspective 集成
- **配置位置**: `init-ui.el`

### treemacs-tab-bar
- **功能**: Treemacs 与 Tab Bar 集成
- **配置位置**: `init-ui.el`

### dirvish
- **功能**: 现代化的 Dired 增强
- **配置位置**: `init-keybindings.el`
- **快捷键**: `SPC e` - 切换 dirvish
- **特性**:
  - 文件预览
  - 图标支持
  - NeoTree 风格操作
  - 媒体文件支持

### winum
- **功能**: 窗口编号，快速切换窗口
- **配置位置**: `init-ui.el`

---

## 编程支持

### company
- **功能**: 自动补全框架
- **配置位置**: `init-packages.el`
- **设置**:
  - 最小前缀长度: 1
  - 无延迟补全
  - `C-n`/`C-p` 选择候选项

### avy
- **功能**: 快速跳转到可见位置
- **配置位置**: `init-packages.el`, `init-keybindings.el`
- **快捷键**:
  - `S` - 跳转到单词
  - `s` - 跳转到字符

### smartparens
- **功能**: 智能括号配对和操作
- **配置位置**: `init-code.el`
- **快捷键**:
  - `<tab>` - 跳出括号
  - `<backtab>` - 跳回括号

### hl-todo
- **功能**: 高亮 TODO、FIXME 等关键词
- **配置位置**: `init-code.el`
- **关键词**: TODO, FIXME, NOTE, BUG

---

## LSP 与语法检查

### lsp-mode
- **功能**: Language Server Protocol 客户端
- **配置位置**: `init-lsp.el`, `init-keybindings.el`
- **支持语言**:
  - C/C++ (clangd)
  - Python (pylsp)
  - Bash
  - Lua
- **快捷键前缀**: `SPC c`

### lsp-ui
- **功能**: LSP 的 UI 增强
- **配置位置**: `init-lsp.el`
- **特性**:
  - 悬浮文档
  - 边栏提示
  - 代码预览
- **快捷键**:
  - `gd` - 查看定义
  - `gr` - 查看引用
  - `K` - 查看文档

### lsp-treemacs
- **功能**: LSP 与 Treemacs 集成
- **配置位置**: `init-lsp.el`, `init-keybindings.el`
- **快捷键**: `SPC cs` - 符号列表

### treesit-auto
- **功能**: 自动配置 Tree-sitter
- **配置位置**: `init-lsp.el`

### flymake
- **功能**: 实时语法检查（内置）
- **配置位置**: `init-lsp.el`
- **快捷键**:
  - `C-c f n` - 下一个错误
  - `C-c f p` - 上一个错误
  - `[d`/`]d` - 错误导航

### sideline
- **功能**: 侧边栏信息显示框架
- **配置位置**: `init-lsp.el`

### sideline-flymake
- **功能**: 在侧边栏显示 Flymake 诊断
- **配置位置**: `init-lsp.el`

---

## 代码格式化

### elisp-autofmt
- **功能**: Emacs Lisp 自动格式化
- **配置位置**: `init-lsp.el`
- **特性**: 保存时自动格式化

### lua-mode
- **功能**: Lua 语言支持
- **配置位置**: `init-lsp.el`

---

## Snippet 系统

### yasnippet
- **功能**: 代码片段模板系统
- **配置位置**: `init-packages.el`
- **激活**: 在编程模式下自动启用

### yasnippet-snippets
- **功能**: 官方 snippet 集合
- **配置位置**: `init-packages.el`
- **包含**: 主流编程语言的常用片段

### laas
- **功能**: LaTeX 快速输入系统
- **配置位置**: `init-ultisnippet.el`
- **特性**:
  - 数学符号快速输入
  - 环境快速插入
  - 智能吸取功能

---

## Org 模式

### org
- **功能**: Org 模式核心（通过 straight 安装最新版）
- **配置位置**: `init-org.el`
- **特性**:
  - 笔记管理
  - 任务管理
  - LaTeX 支持
  - 代码块执行

### org-contrib
- **功能**: Org 扩展包集合
- **配置位置**: `init-packages.el`

### org-superstar
- **功能**: 美化 Org 标题和列表
- **配置位置**: `init-org.el`
- **特性**:
  - 自定义标题符号
  - 列表项美化
  - 多层级颜色

### org-download
- **功能**: 图片处理和插入
- **配置位置**: `init-org.el`
- **快捷键**:
  - `C-c i i` - 图片管理器
  - `C-c i f` - 插入文件
  - `C-c i u` - 插入 URL
  - `C-c i s` - 截图
  - `SPC ip` - 粘贴剪贴板图片

### org-bars
- **功能**: Org 标题栏美化
- **配置位置**: `init-org.el`
- **来源**: GitHub - tonyaldon/org-bars

### org-checklist
- **功能**: 检查列表功能增强
- **配置位置**: `init-org.el`
- **特性**:
  - TODO 状态管理
  - 日志记录

### org-fancy-priorities
- **功能**: 优先级图标美化
- **配置位置**: `init-org.el`
- **图标**: 🔴 (A), 🟠 (B), 🟡 (C)

### org-modern-indent
- **功能**: Org 缩进美化（自定义模块）
- **配置位置**: `init.el`

---

## LaTeX 支持

### cdlatex
- **功能**: LaTeX 快速输入
- **配置位置**: `init-org.el`
- **特性**:
  - 数学符号快速输入（`` ` `` 触发）
  - 数学修饰符（`'` 触发）
  - 环境快速插入
  - 命令快速插入
- **快捷键**:
  - `C-c {` - 插入环境
  - `C-c m` - 插入数学符号
  - `C-c '` - 数学修饰符

---

## PDF 查看

### pdf-tools
- **功能**: PDF 阅读器
- **配置位置**: `init-ui.el`
- **特性**:
  - 高质量渲染
  - 注释支持
  - 自动刷新
  - Fit-width 显示

---

## 启动页

### dashboard
- **功能**: Emacs 启动页
- **配置位置**: `init-ui.el`
- **显示内容**:
  - 最近文件（5个）
  - 书签（5个）
  - 项目（10个）
  - 自定义图片横幅

---

## 版本控制

### magit-todos
- **功能**: 在 Magit 中显示 TODO 注释
- **配置位置**: `init-packages.el`

---

## 输入法

### sis
- **功能**: 智能输入源切换
- **配置位置**: `init-packages.el`
- **支持平台**:
  - macOS (ABC + SCIM.ITABC)
  - Linux (fcitx5)
- **特性**:
  - 光标颜色指示
  - Evil 模式集成
  - 上下文感知

---

## Undo 系统

### undo-fu
- **功能**: 简单线性的 undo 系统
- **配置位置**: `init-better-defaults.el`
- **快捷键**:
  - `u` - 撤销
  - `C-r` - 重做
  - `U` - 仅重做

### undo-fu-session
- **功能**: 持久化 undo 历史
- **配置位置**: `init-better-defaults.el`
- **特性**: 保存 undo 历史到磁盘

### vundo
- **功能**: 可视化 undo 树
- **配置位置**: `init-better-defaults.el`
- **快捷键**: `g u` - 打开 undo 树
- **导航**: `hjkl` - Vim 风格导航

---

## AI 辅助

### copilot
- **功能**: GitHub Copilot AI 代码补全
- **配置位置**: `init-code.el`
- **来源**: GitHub - zerolfx/copilot.el
- **快捷键**:
  - `M-p` - 接受补全
  - `SPC ae` - 切换模式

### copilot-chat
- **功能**: GitHub Copilot 聊天界面
- **配置位置**: `init-code.el`
- **来源**: GitHub - chep/copilot-chat.el
- **快捷键**: `SPC aa` - 打开聊天

---

## 内置包配置

### recentf
- **功能**: 最近访问文件列表（内置）
- **配置位置**: `init-packages.el`
- **设置**: 最多显示 10 个项目

### simple
- **功能**: 基本编辑功能（内置）
- **配置位置**: `init-packages.el`
- **启用**:
  - 列号显示
  - 文件大小显示

### savehist
- **功能**: 保存 minibuffer 历史（内置）
- **配置位置**: `init-packages.el`
- **设置**:
  - 历史长度: 1000
  - 自动保存间隔: 300 秒

### saveplace
- **功能**: 记住文件中的光标位置（内置）
- **配置位置**: `init-packages.el`

---

## 自定义模块

### compiler
- **功能**: 自定义编译器模块
- **配置位置**: `init.el`, `init-keybindings.el`
- **快捷键前缀**: `SPC m`
- **功能**:
  - `mm` - 智能运行
  - `mt` - 打开/切换结果
  - `mr` - 重新运行
  - `ms` - 停止
  - `mc` - 清理输出

### laas-org-math-detection
- **功能**: LaTeX 数学环境检测
- **配置位置**: `init.el`, `init-org.el`

### org-bars
- **功能**: Org 标题栏美化（自定义）
- **配置位置**: `init.el`

### org-modern-indent
- **功能**: Org 现代缩进样式（自定义）
- **配置位置**: `init.el`

---

## 统计信息

- **总计插件数**: 70+
- **包管理器**: straight.el + use-package
- **配置文件数**: 14 个
- **主要语言支持**: C/C++, Python, Lua, Bash, Emacs Lisp, LaTeX
- **主要用途**: 编程、笔记、文档编写、任务管理

---

## 快捷键前缀说明

| 前缀 | 功能域 |
|------|--------|
| `SPC f` | 文件操作 |
| `SPC b` | 缓冲区操作 |
| `SPC s` | 搜索 |
| `SPC w` | 窗口管理 |
| `SPC c` | 代码/LSP |
| `SPC g` | Git |
| `SPC h` | 帮助 |
| `SPC o` | 打开 |
| `SPC t` | 切换功能 |
| `SPC x` | 诊断 |
| `SPC m` | 编译器 |
| `SPC a` | AI/Agenda |
| `SPC i` | 图片操作 |

---

## 配置文件结构

```
.emacs.d/
├── init.el                      # 主配置入口
├── custom.el                    # 自定义设置
└── lisp/
    ├── init-packages.el         # 包管理和安装
    ├── init-ui.el              # UI 配置
    ├── init-better-defaults.el  # 默认设置优化
    ├── init-keybindings.el     # 快捷键绑定
    ├── init-org.el             # Org 模式配置
    ├── init-lsp.el             # LSP 配置
    ├── init-code.el            # 编程相关
    ├── init-ultisnippet.el     # Snippet 配置
    ├── compiler.el             # 编译器模块
    ├── laas-org-math-detection.el
    ├── org-bars.el
    └── org-modern-indent.el
```

---

## 更新日期

最后更新: 2025-10-18

---

## 注意事项

1. 某些插件需要外部依赖（如 LSP servers、ripgrep、fd 等）
2. macOS 和 Linux 平台有部分平台特定配置
3. 字体依赖: Maple Mono NF, Noto Serif CJK SC
4. Python LSP 配置路径: `~/.venv/bin/pylsp`
5. Git 仓库需要初始化才能使用 Magit 相关功能
