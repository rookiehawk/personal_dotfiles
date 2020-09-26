set cmdheight=2
set conceallevel=0
set nu
syntax on
syntax enable
set autoindent
let mapleader = "\<space>"
set ruler " 在状态栏显示光标的当前位置，位于哪一行那一列
" set cursorline
set fileencodings=utf-8,ucs-bom,gb18030,gbk,gb2312,cp936,chinese
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
set clipboard=unnamed
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
"        set guifont=Monaco:h13,Consolas:h13,Courier_New:h11:cANSI
        set guifont=Liberation Mono:h13,Consolas:h13,Courier_New:h11:cANSI
    else
        set guifont=JetBrainsMono\ Nerd\ Font\ Mono\ 13
    endif
endif
" set guifont=Andale\ Mono\ 11 这是linux设置gvim字体的格式
" set guifont=Monaco:h11 Mac风格
" set guifont=Andale_Mono:h11 Win风格

call plug#begin('~/.vim/plugged')

Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Shorthand notation; fetches https://github.com/junegunn/vim-easy-align
Plug 'junegunn/vim-easy-align'

Plug 'scrooloose/nerdtree'

" Plugin outside ~/.vim/plugged with post-update hook
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

Plug 'valloric/youcompleteme'

Plug 'easymotion/vim-easymotion'

Plug 'Yggdroot/indentLine'

Plug 'jiangmiao/auto-pairs'

Plug 'mhinz/vim-startify'

Plug 'ryanoasis/vim-devicons'

Plug 'majutsushi/tagbar'

Plug 'kien/ctrlp.vim'

Plug 'morhetz/gruvbox'

Plug 'tpope/vim-surround'

Plug 'airblade/vim-gitgutter'

Plug 'pangloss/vim-javascript'

Plug 'mattn/emmet-vim'
" Initialize plugin system
call plug#end()


" =================== configuration ====================

" airline
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#formatter = 'default'
let g:airline_theme='violet'
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'


" ================== tagbar =============================
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


" ================== fzf ============================
" Empty value to disable preview window altogether
let g:fzf_preview_window = ''

" Always enable preview window on the right with 60% width
let g:fzf_preview_window = 'right:60%'

" [Buffers] Jump to the existing window if possible
let g:fzf_buffers_jump = 1

" [[B]Commits] Customize the options used by 'git log':
let g:fzf_commits_log_options = '--graph --color=always --format="%C(auto)%h%d %s %C(black)%C(bold)%cr"'

" [Tags] Command to generate tags file
let g:fzf_tags_command = 'ctags -R'

" [Commands] --expect expression for directly executing the command
let g:fzf_commands_expect = 'alt-enter,ctrl-x'

" Mapping selecting mappings
nmap <leader><tab> <plug>(fzf-maps-n)
xmap <leader><tab> <plug>(fzf-maps-x)
omap <leader><tab> <plug>(fzf-maps-o)

" Insert mode completion
imap <c-x><c-k> <plug>(fzf-complete-word)
imap <c-x><c-f> <plug>(fzf-complete-path)
imap <c-x><c-j> <plug>(fzf-complete-file-ag)
imap <c-x><c-l> <plug>(fzf-complete-line)

" Advanced customization using autoload functions
inoremap <expr> <c-x><c-k> fzf#vim#complete#word({'left': '15%'})

" ================= ycm ================================
" YouCompleteMe
" Python Semantic Completion
let g:ycm_python_binary_path = '/usr/bin/python3'
" C family Completion Path
let g:ycm_global_ycm_extra_conf='~/.ycm_extra_conf.py'
" 跳转快捷键
nnoremap <c-k> :YcmCompleter GoToDeclaration<CR>|
nnoremap <c-h> :YcmCompleter GoToDefinition<CR>| 
nnoremap <c-j> :YcmCompleter GoToDefinitionElseDeclaration<CR>|
" 停止提示是否载入本地ycm_extra_conf文件
let g:ycm_confirm_extra_conf = 0
" 语法关键字补全
let g:ycm_seed_identifiers_with_syntax = 1
" 开启 YCM 基于标签引擎
let g:ycm_collect_identifiers_from_tags_files = 1
" 从第2个键入字符就开始罗列匹配项
let g:ycm_min_num_of_chars_for_completion=2
" 在注释输入中也能补全
let g:ycm_complete_in_comments = 1
" 在字符串输入中也能补全
let g:ycm_complete_in_strings = 1
" 注释和字符串中的文字也会被收入补全
let g:ycm_collect_identifiers_from_comments_and_strings = 1
" 弹出列表时选择第1项的快捷键(默认为<TAB>和<Down>)
let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
" 弹出列表时选择前1项的快捷键(默认为<S-TAB>和<UP>)
let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
" 主动补全, 默认为<C-Space>
"let g:ycm_key_invoke_completion = ['<C-Space>']
" 停止显示补全列表(防止列表影响视野), 可以按<C-Space>重新弹出
"let g:ycm_key_list_stop_completion = ['<C-y>']


" ================= javacript vim =============================================
let g:javascript_plugin_jsdoc = 1

" ================== emmet vim ===================================
let g:user_emmet_mode='n'    "only enable normal mode functions.
let g:user_emmet_mode='inv'  "enable all functions, which is equal to
let g:user_emmet_mode='a'    "enable all function in all mode.

let g:user_emmet_install_global = 0
autocmd FileType html,css EmmetInstall


