#-------------------------------------------------------------------------------------------------------------
# Copyright (c) Microsoft Corporation. All rights reserved.
# Licensed under the MIT License. See https://go.microsoft.com/fwlink/?linkid=2090316 for license information.
#-------------------------------------------------------------------------------------------------------------

FROM node:lts

# Avoid warnings by switching to noninteractive
ENV DEBIAN_FRONTEND=noninteractive

# Configure apt and install packages
RUN apt-get update \
    && apt-get -y install --no-install-recommends apt-utils 2>&1 \ 
    #
    # Verify git and needed tools are installed
    && apt-get install -y git procps \
    #
    # Remove outdated yarn from /opt and install via package 
    # so it can be easily updated via apt-get upgrade yarn
    && rm -rf /opt/yarn-* \
    && rm -f /usr/local/bin/yarn \
    && rm -f /usr/local/bin/yarnpkg \
    && apt-get install -y curl apt-transport-https lsb-release \
    && curl -sS https://dl.yarnpkg.com/$(lsb_release -is | tr '[:upper:]' '[:lower:]')/pubkey.gpg | apt-key add - 2>/dev/null \
    && echo "deb https://dl.yarnpkg.com/$(lsb_release -is | tr '[:upper:]' '[:lower:]')/ stable main" | tee /etc/apt/sources.list.d/yarn.list \
    && apt-get update \
    && apt-get -y install --no-install-recommends yarn \
    # Install additional packages
    && apt-get -y install zsh jq tmux sudo less vim \
    #
    # Install eslint globally
    && npm install -g eslint \
    #
    # Clean up
    && apt-get autoremove -y \
    && apt-get clean -y \
    && rm -rf /var/lib/apt/lists/*

# Install elm
# Set elm version
ARG ELM_VERSION=0.19.0

RUN yarn global add elm@${ELM_VERSION} elm-test elm-css elm-live elm-format

# Add devgroup & devuser
RUN groupadd -g 999 devgroup \
    && useradd -u 999 -g devgroup -G sudo -m -s /usr/bin/zsh devuser \
    && passwd -d devuser

# RUN echo 'Defaults visiblepw'             >> /etc/sudoers
RUN echo 'devuser ALL=(ALL) NOPASSWD:ALL' >> /etc/sudoers.d/devuser

# Change user
USER devuser

# Install prezto
RUN zsh -c 'git clone --recursive https://github.com/sorin-ionescu/prezto.git "${ZDOTDIR:-$HOME}/.zprezto"'
RUN zsh -c 'setopt EXTENDED_GLOB && for rcfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/^README.md(.N); do ln -s "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile:t}"; done'

# Set prompt theme
ENV PREZTO_PROMPT_THEME=cloud
RUN zsh -c 'source "${ZDOTDIR:-$HOME}/.zshrc" && prompt -s "${PREZTO_PROMPT_THEME}" | sed "1,3d" >> "${ZDOTDIR:-$HOME}/.zshrc"'

# Create Symbolic link to workspace
RUN ln -s /workspaces ~/workspaces

# Switch back to dialog for any ad-hoc use of apt-get
ENV DEBIAN_FRONTEND=dialog
