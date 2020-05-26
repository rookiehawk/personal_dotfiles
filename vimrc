set cmdheight=2
set conceallevel=0
set nu
syntax on
syntax enable
set autoindent
let mapleader = "\<space>"
set ruler " 在状态栏显示光标的当前位置，位于哪一行那一列
" set cursorline
set fileencodings=utf-8,ucs-bom,gb18030,gbk,gb2312,cp936
set termencoding=utf-8
set encoding=utf-8
" 鼠标
set mouse=a
set selection=exclusive
set selectmode=mouse,key
set showmatch
" set paste
set nocompatible
set showmode " 在底部显示，当前处于的模式
set showcmd " 在底部显示当前键入的指令
set t_Co=256
filetype indent on " 开启文件类型检查，并且载入与该类型对应的缩进规则。比如，如果编辑的是.py文件，Vim 就是会找 Python 的缩进规则~/.vim/indent/python.vim
set textwidth=80 " 设置行宽，一行显示多少字符
set wrap " 自动折行，太长的话分成几行显示
" set nowrap " 关闭自动折行
set linebreak " 只用遇到制定的符号（空格、连词号等标点符号），才发生折行，不会再单词内部折行
set wrapmargin=2 " 制定折行处与编辑窗口的右边缘之间空出的字符数
set scrolloff=5 " 垂直滚动时，光标距离顶部/底部的位置（单位：行）
set sidescrolloff=15 " 水平滚动时，光标距离行首或行尾的位置（单位：字符），该配置在不折行时比较有用
set laststatus=2 " 是否显示状态栏，2表示显示 0 不显示 1 多窗口才显示

" =============== 搜索 ================
set hlsearch " 高量显示搜索结果
set incsearch " 每输入一个字符，就自动跳到第一个配置的结果
set ignorecase " 忽略大小写
set smartcase " 如果同时打开了ignorecase，那么对于只有一个大写字母的搜索词，将大小写敏感

" ================= 编辑 ==================
" set spell spelllang=en_us " 拼写检查 英语
set nobackup " 不创建备份文件 该文件的标志是，原文件名的末尾，加了一个波浪号
set noswapfile " 不创建交换文件，交换文件的作用是系统崩溃时恢复文件，文件的开头是. 结尾是.swp
set undofile " 保留撤销历史
set undodir=~/.vim/.undo//
set autochdir " 自动切换工作目录
set noerrorbells " 出错时，不要发出响声
set visualbell " 出错时，闪屏
set history=1000 " vim记住多少次历史操作
set autoread " 打开文件监视，如果在编辑过程中文件发生外部改变，会提示
set autowrite
set wildmenu
set wildmode=longest:list,full " 命令模式下，底部操作指令按下tab自动补全，第一次tab，会显示所有匹配的操作指令清单，第二次tab，依次选择各个指令
"共享剪切板"
set clipboard+=unnamed
"文件类型自动检测，代码智能补全"
set completeopt=longest,preview,menu

"隐藏工具栏"
set guioptions-=T
"隐藏菜单栏"
set guioptions-=m
set guioptions-=r
set guioptions-=l
set guioptions-=b 
"==============================================================================
" 主题配色 
"==============================================================================

" 开启24bit的颜色，开启这个颜色会更漂亮一些
" set termguicolors
" set guifont=DejaVu\ Sans\ Mono\ for\ Powerline\ Book\ :h13:cANSI
set t_Co=256
colorscheme gruvbox
set background=dark

" =================== map 映射 ============================



"=================== 显示中文帮助 ==========================

if version >= 603
        set helplang=cn
            set encoding=utf-8
endif


" ================= guifont ================================
if has('gui_running')
    if has("win16") || has("win32") || has("win95") || has("win64")
        set guifont=Consolas:h13,Courier_New:h11:cANSI
    else
        set guifont=Hack\ Nerd\ Font\ Mono\ 13
    endif
endif
" set guifont=Andale\ Mono\ 11 这是linux设置gvim字体的格式
" set guifont=Monaco:h11 Mac风格
" set guifont=Andale_Mono:h11 Win风格

call plug#begin('~/.vim/plugged')

" Shorthand notation; fetches https://github.com/junegunn/vim-easy-align
Plug 'junegunn/vim-easy-align'

Plug 'scrooloose/nerdtree'

" Plugin outside ~/.vim/plugged with post-update hook
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

Plug 'valloric/youcompleteme'

Plug 'easymotion/vim-easymotion'

Plug 'itchyny/lightline.vim'

Plug 'Yggdroot/indentLine'

Plug 'jiangmiao/auto-pairs'

Plug 'mhinz/vim-startify'

Plug 'ryanoasis/vim-devicons'

Plug 'majutsushi/tagbar'

Plug 'kien/ctrlp.vim'

Plug 'morhetz/gruvbox'
Plug 'shinchu/lightline-gruvbox.vim'

Plug 'tpope/vim-surround'

Plug 'airblade/vim-gitgutter'

" Initialize plugin system
call plug#end()


" =================== configuration ====================
" indentline
let g:indentLine_char_list = ['|', '¦', '┆', '┊']

" tagbar
" 设置tagbar快捷键
nmap <F8> :TagbarToggle<CR>  
let g:tagbar_ctags_bin='/usr/bin/ctags'  " 设置ctags所在路径
" 需要下载ctags
" auto-pairs
" fly mode
let g:AutoPairsFlyMode = 1
let g:AutoPairsShortcutBackInsert = '<M-b>'

" nerdtree
map <C-n> :NERDTreeToggle<CR>

" vim-easy-align
nmap ga <Plug>(EasyAlign)
xmap ga <Plug>(EasyAlign)

" easymotion
let g:EasyMotion_do_mapping = 0 " Disable default mappings

" Jump to anywhere you want with minimal keystrokes, with just one key binding.
" `s{char}{label}`
nmap s <Plug>(easymotion-overwin-f)
" or
" `s{char}{char}{label}`
" Need one more keystroke, but on average, it may be more comfortable.
nmap s <Plug>(easymotion-overwin-f2)

" Turn on case-insensitive feature
let g:EasyMotion_smartcase = 1

" JK motions: Line motions
map <Leader>j <Plug>(easymotion-j)
map <Leader>k <Plug>(easymotion-k)

" ctrlp
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
let g:ctrlp_working_path_mode = 'ra'

" lightline-gruvbox.vim
let g:lightline = {}
let g:lightline.colorscheme = 'gruvbox'
