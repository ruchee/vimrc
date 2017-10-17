from .base import Base
from neovim.api.nvim import NvimError

class Source(Base):
    def __init__(self, vim):
        Base.__init__(self, vim)

        self.name = 'phpcd'
        self.mark = '[phpcd]'
        self.filetypes = ['php']
        self.is_bytepos = True
        self.input_pattern = '\w+|[^. \t]->\w*|\w+::\w*|\\\\'
        self.rank = 500
        self.max_pattern_length = -1
        self.matchers = ['matcher_full_fuzzy']

    def get_complete_position(self, context):
        return self.vim.call('phpcd#CompletePHP', 1, '')

    def gather_candidates(self, context):
        try:
            return self.vim.call('phpcd#CompletePHP', 0, context['complete_str'])
        except NvimError:
            return []
