FROM nixos/nix

RUN git clone https://github.com/mappl-pldi24-ae/mappl-pldi24-ae --branch=docker
WORKDIR /mappl-pldi24-ae
RUN nix --extra-experimental-features  nix-command --extra-experimental-features flakes develop 

# ENV LC_ALL en_US.UTF-8
# ENV LANG en_US.UTF-8
# ENV LANGUAGE en_US:en

CMD ["nix", "--extra-experimental-features",  "nix-command", "--extra-experimental-features", "flakes", "develop"]
