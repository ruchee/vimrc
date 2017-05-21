"use strict"
/**
 * Generate after/syntax/lisp.vim syntax file based on the config file syntax_mapping.yml
 *
 * @author Nguyen Nguyen <NLKNguyen@MSN.com>
 * @origin https://github.com/NLKNguyen/vim-lisp-syntax
 * @license MIT
 */
const YAML = require('yamljs');
const _    = require('lodash');
const fs   = require('fs');

const syntaxMapping = YAML.load('syntax_mapping.yml');

const syntaxGroups  = [];
const highlightLinks = [];

_.forOwn(syntaxMapping, (group, name) => {
  let keywords       = group['keywords'];
  if (keywords) {
    keywords = keywords.split(/[\s\n\t]+/).filter(Boolean);
  }
  let matches       = group['matches'];
  if (matches) {
    matches = matches.split(/[\n]+/).filter(Boolean);
  }
  const highlight_link = group['highlight_link'];

  _.forEach(keywords, (keyword) => {
    syntaxGroups.push(String.raw`syn keyword ${name} ${keyword}`);
  });

  _.forEach(matches, (match) => {
    syntaxGroups.push(String.raw`syn match ${name} ${ _.trim(match) }`);
  });

  if (highlight_link) {
    highlightLinks.push(`hi link ${name} ${highlight_link}`);
  }
});

////////////////////////////////
// Write to file

const file = fs.createWriteStream('after/syntax/lisp.vim', { flags: 'w'} );

file.on('error', (err) => { /* error handling */ });

file.write(`
" This is an automatically generated syntax file created on ${(new Date()).toUTCString()}
" Origin: https://github.com/NLKNguyen/vim-lisp-syntax
`);

_.forEach(syntaxGroups, (line) => {
  file.write(line + '\n');
});

_.forEach(highlightLinks, (line) => {
  file.write(line + '\n');
});

file.end();
