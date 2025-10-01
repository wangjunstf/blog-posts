---
title: Emacs 配置C++开发环境
date: 2025-09-23 17:06:50
updated:
categories: [Emacs]
tags: [Emacs]
---
Emacs 入门和命令查询：[Emacs 入门教程](https://wangjunstf.github.io/2025/09/22/emacs-ru-men-jiao-cheng/)

熟悉了Emacs的基本使用之后，我们现在来尝试使用 Emacs 配置一个比较现代、实用的C++开发环境（包括代码补全、跳转、语法检查、调试等）
<!-- more -->
Emacs 的配置文件为：`~/.emacs.d/init.el`
将下列的配置信息保存到 Emacs 的配置文件中，保存并重启打开 Emacs 就可以生效。

## 安装必要的包管理器
```lisp
;; 包管理器初始化 
(require 'package) ; 加载Emacs包管理系统

;; 设置包镜像源为清华大学TUNA镜像，加快国内下载速度 
(setq package-archives
      '(("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)  ; 初始化包系统

;; 确保use-package已安装（用于声明式包管理）
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
  
;; 确保在安装 use-package 前刷新包列表
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
```

## 基本的 C++ 模式支持
Emacs 自带 `cc-mode`主模式，支持C、C++、Java、Objective-C、AWK 等一系列在语法和风格上类似的“类C”语言的语法高亮和智能缩进。

安装 clang-format
针对Ubuntu:
```
sudo apt install clang-format
```
 Mac： `brew install clang-format` 
 Windows： 用 LLVM 安装包

我们可以安装 `clang-format` 实现代码自动排版：
```lisp
(use-package clang-format
  :ensure t
  :commands (clang-format-buffer clang-format-region)
  :init
  
  ;; 设置样式
  (setq clang-format-style "file")   ; 使用项目中的 .clang-format 文件
  (setq clang-format-fallback-style "llvm")  ; 无配置文件时的默认样式
  
  ;; 启用详细日志
  ;; (setq clang-format-verbose t)
  
  ;; 绑定快捷键
  :bind (("C-c f" . clang-format-buffer)      ; 针对整个缓冲区
         ("C-c r" . clang-format-region)))    ; 针对选中的区域
```
我们可以在 Emacs 配置一个钩子函数，每次保存时都对代码自动自动排版
```lisp
;; 每次保存时都自动格式化代码
(add-hook 'c-mode-common-hook
          (lambda ()
            (add-hook 'before-save-hook 'clang-format-buffer nil t)))
```

## 代码补全：LSP + clangd
最主流的方法是用 **LSP（Language Server Protocol）** 与 clangd 配合。LSP (Language Server Protocol) 是一种​**​开放标准协议​**​，它允许编辑器或 IDE 与语言服务器进行通信，从而提供智能代码功能。在 Emacs 中，LSP 通过特定的包实现，将编辑器转变为功能强大的现代开发环境。

**Emacs 配置**
推荐 `lsp-mode` + `lsp-ui`：
```lisp
;; 代码补全：LSP+clang
(use-package lsp-mode
  :hook ((c++-mode . lsp)
         (c-mode . lsp))
  :init
  ;; 用 clangd 作为后端
  (setq lsp-clients-clangd-executable "clangd")
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode)
```
这样就有：智能补全、跳转、悬停文档、诊断信息等。关于 LSP 的详细使用，包括快捷键绑定等，后面写文章总结。

## 自动补全：company-mode
```lisp
(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0))  ;; 输入就弹出
```
例如：
```
std::vector<int> myvec;
```
当输入 `myvec.`的时候，它会弹出一个框，选择相应的成员函数等。


## 语法检查：flycheck
```lisp
(use-package flycheck
  :hook (prog-mode . flycheck-mode))

```

## 调试：GDB/ dap-mode
Emacs 自带 `M-x gdb`，也可以用 `dap-mode`（VSCode 风格的调试器前端）：
```lisp
(use-package dap-mode
  :after lsp-mode
  :config
  (dap-auto-configure-mode))

```

## 项目管理：project.el / projectile
```lisp
(use-package projectile
  :init (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map))
```
projectile包是一个强大的项目管理和导航工具，详细用法后面写专题文章总结。

## 模板与 Snippets
这个配置用于设置 Emacs 的 ​**​YASnippet​**​ 系统，这是一个强大的代码片段管理工具，可以显著提高编码效率。
```
(use-package yasnippet
  :hook (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets) ;; 常用代码片段集合
```

## 完整配置文件
``` lisp
;;; init.el --- My Emacs configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 编程之禅

;; URL: https://github.com/wangjunstf/emacs-config
;; Version: 1.0

(require 'package)

;; code
(menu-bar-mode -1)
(tool-bar-mode -1)

(setq package-archives
      '(("gnu"   . "https://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa" . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))
(package-initialize)

;; 确保在安装 use-package 前刷新包列表
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; 正确加载 use-package
(require 'use-package)
(setq use-package-always-ensure t)

;; 基本C++模式支持
(use-package clang-format
  :ensure t
  :commands (clang-format-buffer clang-format-region)
  :init
  
  ;; 设置样式
  (setq clang-format-style "file")   ; 使用项目中的 .clang-format 文件
  (setq clang-format-fallback-style "llvm")  ; 无配置文件时的默认样式
  
  ;; 启用详细日志
  ;; (setq clang-format-verbose t)
  
  :bind (("C-c f" . clang-format-buffer)
         ("C-c r" . clang-format-region)))

;; 每次保存时都自动格式化代码
(add-hook 'c-mode-common-hook
          (lambda ()
            (add-hook 'before-save-hook 'clang-format-buffer nil t)))

;; 代码补全：LSP+clang
(use-package lsp-mode
  :ensure t ; 确保安装包
  :hook ((c++-mode . lsp)
         (c-mode . lsp))
  :init
  ;; 用 clangd 作为后端
  (setq lsp-clients-clangd-executable "clangd")
  :commands lsp)

(use-package lsp-ui
  :commands lsp-ui-mode)


;; 自动补全
(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0))  ;; 输入就弹出

;; 语法检查
(use-package flycheck
  :hook (prog-mode . flycheck-mode))


;; 调试：GDB
(use-package dap-mode
  :after lsp-mode
  :config
  (dap-auto-configure-mode))

;; 项目管理
(use-package projectile
  :init (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map))

```