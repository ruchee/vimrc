# vim-kubernetes

This package provides kubernetes YAML snippets, as well as a growing number of
integrations with kubectl.

## Current integrations:

### Functions/Commands
For the current buffer (including modifications not on disk)
- KubeApply
- KubeDelete
- KubeCreate

And for the current directory (read from disk)
- KubeApplyDir
- KubeDeleteDir

# TODO
- More kubectl integration
- Autocompletion based on cluster resources and legal k8s types
