set nu
set autoindent
syntax enable " 开启语法高亮功能
syntax on " 允许用指定语法高亮配色方案替换默认方案
set ruler
set cmdheight=2
set nocompatible
filetype on " 开启文件类型侦测
filetype plugin on " 根据侦测到的不同类型加载对应的插件
" 开启实时搜索功能
set incsearch
" 搜索时大小写不敏感
set ignorecase
" 关闭兼容模式
set nocompatible
" vim 自身命令行模式智能补全
set wildmenu

set clipboard=unnamed

" 设置 gvim 显示字体
set guifont=DejaVu\ Sans\ Mono\ Book\ 12.5
set magic                   " 设置魔术
set guioptions-=T           " 隐藏工具栏
set guioptions-=m           " 隐藏菜单栏

" 禁止折行
set nowrap

" 自适应不同语言的智能缩进
filetype indent on
" 将制表符扩展为空格
set expandtab
" 设置编辑时制表符占用空格数
set tabstop=4
" 设置格式化时制表符占用空格数
set shiftwidth=4
" 让 vim 把连续数量的空格视为一个制表符
set softtabstop=4


set showmode
set showcmd

set showmatch " 光标遇到圆括号、方括号、大括号时，自动高亮对应的另一个圆括号、方括号和大括号
set hlsearch
set incsearch
set ignorecase
set smartcase


set mouse=a

set t_Co=256

set autochdir


set noerrorbells
set visualbell

set wrap " 自动折行，太长的行分成几行显示
set textwidth=80


" 如果行尾有多余的空格（包括 Tab 键），该配置将让这些空格显示成可见的小方块
set listchars=tab:»■,trail:■
set list

" 命令模式下，底部操作指令按下 Tab 键自动补全。第一次按下 Tab，会显示所有匹配的操作指令的清单；第二次按下 Tab，会依次选择各个指令
set wildmenu
set wildmode=longest:list,full

" 让配置变更立即生效
autocmd BufWritePost $MYVIMRC source $MYVIMRC

