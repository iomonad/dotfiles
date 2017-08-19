" Vim filetype detection file
" Language:	Gentoo Things
" Author:	Ciaran McCreesh <ciaranm@gentoo.org>
" Copyright:	Copyright (c) 2004-2005 Ciaran McCreesh
" Licence:	You may redistribute this under the same terms as Vim itself
"
" This sets up syntax highlighting for Gentoo ebuilds, eclasses, GLEPs and
" Gentoo style ChangeLogs.
"

if &compatible || v:version < 603
    finish
endif


" ebuilds, eclasses
au BufNewFile,BufRead *.e{build,class}
    \     set filetype=ebuild

" GLEPs
au BufNewFile,BufRead *.txt
    \ if (getline(1) =~? "^GLEP: ") |
    \     set filetype=glep |
    \ endif

" ChangeLogs
au BufNewFile,BufRead ChangeLog*
    \ if (getline(2) =~? "^# Copyright \\d\\+-\\d\\+ Gentoo Foundation") |
    \     set filetype=gentoo-changelog |
    \ endif

" /etc/init.d/ scripts
au BufNewFile,BufRead /etc/init.d/*
    \     set filetype=gentoo-init-d |

au BufNewFile,BufRead /*/files/*
    \ if (getline(1) =~? "#!/sbin/\\(runscript\\|openrc-run\\)") |
    \     set filetype=gentoo-init-d |
    \ endif

" /etc/conf.d/ scripts
au BufNewFile,BufRead /etc/conf.d/*
    \     set filetype=gentoo-conf-d

" /etc/env.d/ scripts
au BufNewFile,BufRead /etc/env.d/*
    \     set filetype=gentoo-env-d

" /etc/cron.d/ scripts
au BufNewFile,BufRead /etc/cron.d/*
    \     set filetype=crontab

" package.mask, package.unmask
au BufNewFile,BufRead {*/package.{un,}mask,*/portage/package.{un,}mask/*}
    \     set filetype=gentoo-package-mask

" package.keywords
au BufNewFile,BufRead {*/package.{accept_,}keywords,*/portage/package.{accept_,}keywords/*}
    \     set filetype=gentoo-package-keywords

" package.use
au BufNewFile,BufRead {*/package.use,*/portage/package.use/*}
    \     set filetype=gentoo-package-use

" package.license
au BufNewFile,BufRead {*/package.license,*/portage/package.license/*}
    \     set filetype=gentoo-package-license

" package.properties
au BufNewFile,BufRead {*/package.properties,*/portage/package.properties/*}
    \     set filetype=gentoo-package-properties

" thirdpartymirrors
au BufNewFile,BufRead {*/thirdpartymirrors,*/portage/mirrors}
    \     set filetype=gentoo-mirrors

" make.conf
au BufNewFile,BufRead make.{conf,globals}
    \     set filetype=gentoo-make-conf

" use.desc
au BufNewFile,BufRead use.{local.,}desc
    \     set filetype=gentoo-use-desc

" metadata.xml
au BufNewFile,BufRead metadata.xml
    \     set filetype=gentoo-metadata

" guidexml
au BufNewFile,BufRead *.xml
    \     if getline(1) =~ "<!DOCTYPE \\(guide\\|news\\|mainpage\\|book\\|sections\\|dynamic\\|inserts\\) " ||
    \        getline(2) =~ "<!DOCTYPE \\(guide\\|news\\|mainpage\\|book\\|sections\\|dynamic\\|inserts\\) " ||
    \        getline(3) =~ "<!DOCTYPE \\(guide\\|news\\|mainpage\\|book\\|sections\\|dynamic\\|inserts\\) " ||
    \        getline(4) =~ "<!DOCTYPE \\(guide\\|news\\|mainpage\\|book\\|sections\\|dynamic\\|inserts\\) " ||
    \        getline(5) =~ "<!DOCTYPE \\(guide\\|news\\|mainpage\\|book\\|sections\\|dynamic\\|inserts\\) " ||
    \        getline(6) =~ "<!DOCTYPE \\(guide\\|news\\|mainpage\\|book\\|sections\\|dynamic\\|inserts\\) " ||
    \        getline(7) =~ "<!DOCTYPE \\(guide\\|news\\|mainpage\\|book\\|sections\\|dynamic\\|inserts\\) " ||
    \        getline(8) =~ "<!DOCTYPE \\(guide\\|news\\|mainpage\\|book\\|sections\\|dynamic\\|inserts\\) " ||
    \        getline(9) =~ "<!DOCTYPE \\(guide\\|news\\|mainpage\\|book\\|sections\\|dynamic\\|inserts\\) " |
    \     set filetype=guidexml		      |
    \     endif
