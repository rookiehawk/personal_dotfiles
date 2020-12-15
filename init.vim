set cmdheight=2
set conceallevel=0
set nu
syntax on
syntax enable
set autoindent
set laststatus=2
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
set clipboard=unnamed
set nocompatible
set backspace=2
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

"==============================================================================
" 主题配色 
"==============================================================================
colorscheme gruvbox
set background=dark







call plug#begin('~/.config/nvim/plugged')
" Shorthand notation; fetches https://github.com/junegunn/vim-easy-align
Plug 'junegunn/vim-easy-align'

" vim airline
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" vim startify
Plug 'mhinz/vim-startify'

" indentLine
Plug 'Yggdroot/indentLine'

" nerdtree
Plug 'scrooloose/nerdtree'

" auto-pairs
Plug 'jiangmiao/auto-pairs'

" gitgutter
Plug 'airblade/vim-gitgutter'

" surround
Plug 'tpope/vim-surround'

" vim-easymotion
Plug 'easymotion/vim-easymotion'

" 配合Nerd Font的一些图标
Plug 'ryanoasis/vim-devicons'

Plug 'pangloss/vim-javascript'

" coc-nvim
Plug 'neoclide/coc.nvim', {'branch': 'release'}

Plug 'mattn/emmet-vim'
" Initialize plugin system
call plug#end()




" ============== 插件配置 ================

" ==============  airline =====================
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#formatter = 'default'
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'
let g:airline_theme='luna'
let g:airline_powerline_fonts = 1
" set guifont=DejaVu Sans Mono for Powerline Book:h13
" 首先要安装powerline字体，然后终端使用powerline字体，才会完整显示出
" luna 主题





" ==================  indentLine ===============
let g:indentLine_color_term = 239
let g:indentLine_color_tty_light = 7 " (default: 4)
let g:indentLine_color_dark = 1 " (default: 2)
let g:vim_json_syntax_conceal = 0 "" 为了防止导致json的引号自动隐藏
let g:indentLine_noConcealCursor=""  " 为了防止导致json的引号自动隐藏



" nerdtree
map <C-n> :NERDTreeToggle<CR>
" map <F8> :NERDTreeToggle<CR>
let g:NERDTreeDirArrowExpandable = '▸'
let g:NERDTreeDirArrowCollapsible = '▾'
"
" auto-pairs
let g:AutoPairsFlyMode = 1
let g:AutoPairsShortcutBackInsert = '<M-b>'

" auto-save-file
let g:auto_save = 1  " enable AutoSave on Vim startup



" ================  easy-motion ========================
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
" ===================== easy-motion ===========================



" ================= javacript vim =============================
let g:javascript_plugin_jsdoc = 1

" ================== emmet vim ================================
let g:user_emmet_mode='n'    "only enable normal mode functions.
let g:user_emmet_mode='inv'  "enable all functions, which is equal to
let g:user_emmet_mode='a'    "enable all function in all mode.

let g:user_emmet_install_global = 0
autocmd FileType html,css EmmetInstall
