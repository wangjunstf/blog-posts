# 多语言博客文章仓库 | Multilingual Blog Posts Repository

https://img.shields.io/badge/语言-多语言-blue

https://img.shields.io/badge/状态-持续更新-green

一个集中管理所有语言版本博客文章的仓库，支持中文、英文等多语言内容同步与版本控制。

## 📖 项目简介

本仓库用于系统化管理和维护个人或团队的多语言博客内容，实现：

- ​**​统一存储​**​ - 所有语言版本的文章集中管理
    
- ​**​版本控制​**​ - 使用 Git 追踪内容变更历史
    
- ​**​协作友好​**​ - 支持多人协作编辑和审阅
    
- ​**​自动化部署​**​ - 集成 CI/CD 自动发布到博客平台
    

## 🌍 支持语言

|语言|目录|状态|
|---|---|---|
|简体中文|`zh-CN/`|✅ 活跃维护|
|English|`en-US/`|✅ 活跃维护|
|其他语言|`[lang]/`|🔄 计划支持|

## 📁 仓库结构

```
.
├── zh-CN/                 # 简体中文文章
├── en-US/                 # 英文文章
├── assets/                # 共享资源
│   └── images/           # 图片资源
└── README.md             # 项目说明
```

## 🚀 快速开始

### 克隆仓库

```
git clone git@github.com:wangjunstf/blog-posts.git
cd blog-posts
```

### 添加新文章

1. 选择目标语言目录
    
2. 按分类创建 Markdown 文件
    
3. 使用标准 Front Matter：
    

```
---
title: "文章标题"
date: 2025-10-01 11:20:02
updated:
categories: [分类]
tags: [标签1, 标签2]
---
```
## 🤝 贡献指南

欢迎贡献内容！请遵循：

1. Fork 本仓库
    
2. 创建功能分支：`git checkout -b feature/new-article`
    
3. 提交更改：`git commit -m '添加新文章'`
    
4. 推送到分支：`git push origin feature/new-article`
    
5. 提交 Pull Request
    

## 📝 写作规范

### 文件命名

- 使用英文短横线分隔：`my-article-title.md`
    
- 包含日期：`2024-01-01-article-title.md`
    

### 内容标准

- 中文：使用标准简体中文，注意标点符号规范
    
- 英文：遵循英文写作规范，注意大小写和标点
    
- 图片：统一放在 `assets/images/`并按文章分类
    

## 🔄 自动化流程

集成 GitHub Actions 实现：

- ✅ 自动语法检查
    
- ✅ 拼写错误检测
    
- ✅ 部署到博客平台
    
- ✅ 多语言内容同步验证
    

## 📄 许可证

文章内容版权归作者所有，代码部分采用 MIT 许可证。

---

​**​让写作更有条理，让知识跨越语言边界​**​ 🌟

_最后更新: {更新时间}_
