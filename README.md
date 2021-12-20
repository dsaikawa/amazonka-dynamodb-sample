# amazonka-dynamodb-test
AWS の Dynamodb を使用するための Haskell のライブラリを使用したサンプルです。  
実行するために haskell-stack の動作環境が必要です。  
また、テーブルを作成する際に AWS CLI も使用しています。
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
- AWS CLI を使用して以下の情報のテーブルを作成。
    ``` shell-session
    env AWS_ACCESS_KEY_ID=dummy AWS_SECRET_ACCESS_KEY=dummykey \
    aws dynamodb create-table \
    --region ap-northeast-1 \
    --endpoint-url http://localhost:8000 \
    --table-name fuga \
    --key-schema AttributeName=id,KeyType=HASH \
    --attribute-definitions AttributeName=id,AttributeType=S \
    --billing-mode PAY_PER_REQUEST
    ```
- 作成したテーブル情報
    ``` json
    {
        "TableDescription": {
            "AttributeDefinitions": [
                {
                    "AttributeName": "id",
                    "AttributeType": "S"
                }
            ],
            "TableName": "fuga",
            "KeySchema": [
                {
                    "AttributeName": "id",
                    "KeyType": "HASH"
                }
            ],
            "TableStatus": "ACTIVE",
            "CreationDateTime": 1639964521.578,
            "ProvisionedThroughput": {
                "LastIncreaseDateTime": 0.0,
                "LastDecreaseDateTime": 0.0,
                "NumberOfDecreasesToday": 0,
                "ReadCapacityUnits": 0,
                "WriteCapacityUnits": 0
            },
            "TableSizeBytes": 0,
            "ItemCount": 0,
            "TableArn": "arn:aws:dynamodb:ddblocal:000000000000:table/fuga",
            "BillingModeSummary": {
                "BillingMode": "PAY_PER_REQUEST",
                "LastUpdateToPayPerRequestDateTime": 1639964521.578
            }
        }
    }
    ```

