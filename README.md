# amazonka-dynamodb-test
AWS の Dynamodb を使用するための Haskell のライブラリを使用したサンプルです。  
実行するために haskell-stack の動作環境が必要です。  
# Usage
- このサンプルを実行する際に以下を実行し "zlib1g-dev" ライブラリをインストールしておく必要がある。  
    ``` shell-session
    $ sudo apt-get install zlib1g-dev
    ```
- docker の dynamodb-local イメージを入手してコンテナを起動。
    ``` shell-session
    $ docker pull amazon/dynamodb-local
    $ docker run -d --name dynamodb -p 8000:8000 amazon/dynamodb-local
    ```
- accesskey と secretkey を環境変数に設定する。
    ``` shell session
    $ export AWS_ACCESS_KEY=dummy
    $ export AWS_SECRET_KEY=dummykey
    ```

- 実行
    ``` shell seiion 
    $ stack build
    $ stack run
    ```

