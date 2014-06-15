# Dotfiles

**Dotfiles** helps you get your dev environment up and running.

## What's in the box?

- [Emacs][] via [EVM][]
- [MacVim][]
- [Python][] via [pyenv][]
- [Ruby][] via [rbenv][]
- Support for a lot of programming languages!

## Getting Started

**Get [Ansible][]**

``` sh
sudo easy_install pip
sudo pip install ansible
```

**Pull down the repo**

``` sh
git clone git://github.com/jcf/playbook.git ~/.playbook
cd ~/.playbook
```

**Run the playbook**

``` sh
ansible-playbook main.yml
```

[Ansible]: http://www.ansible.com/
[Dotfiles]: https://github.com/jcf/dotfiles
[EVM]: https://github.com/rejeep/evm
[Emacs]: http://www.gnu.org/software/emacs
[MacVim]: https://code.google.com/p/macvim/
[Python]: https://www.python.org/
[Ruby]: https://www.ruby-lang.org/en/
[pyenv]: https://github.com/yyuu/pyenv
[rbenv]: https://github.com/sstephenson/rbenv
