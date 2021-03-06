FROM centos:7
MAINTAINER rick@elrod.me
WORKDIR /opt
RUN \
  curl -o /etc/yum.repos.d/ghc.repo https://copr.fedorainfracloud.org/coprs/petersen/ghc-8.6.5/repo/epel-7/petersen-ghc-8.6.5-epel-7.repo && \
  yum update -y && \
  yum install -y cabal-install ghc mariadb mariadb-devel gcc gcc-g++
ENTRYPOINT \
  cabal update && \
  cabal install --only-dependencies --constraint="cryptonite -use_target_attributes" && \
  export LC_ALL=en_US.UTF-8 && \
  cabal clean && \
  cabal configure && \
  cabal build
