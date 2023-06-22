# I/O configuration of CPC CPCA

From HMC: HMC_HOST

## Basic CPC information

| Name | Description          | Model    | Mode | IFLs | Mem [GiB] |
|:---- |:-------------------- |:-------- |:---- |:---- |:--------- |
| CPCA | CPCA in pod mypod    | 3907-LR1 | dpm  | 30   | 4032      |

## Adapters of the CPC

| PCHID | Name                 | Description          | Card Type                  |
|:----- |:-------------------- |:-------------------- |:-------------------------- |
| 100   | FCP1.D1              |                      | ficon-express-16s-plus     |
| 101   | FCP1.D2              |                      | ficon-express-16s-plus     |
| 104   | FCP2.D1              |                      | ficon-express-16s-plus     |
| 105   | FCP2.D2              |                      | ficon-express-16s-plus     |
| 108   | OSD1                 |                      | osa-express-6s-1000base-t  |
| 10c   | OSM1                 |                      | osa-express-6s-1000base-t  |
| 110   | OSD2                 |                      | osa-express-6s-10gb        |
| 114   | OSD3                 |                      | osa-express-6s-10gb        |
| 118   | CRYP00               |                      | crypto-express-6s          |
| 11c   | CRYP01               |                      | crypto-express-6s          |
| 128   | OSD4                 |                      | osa-express-6s-1000base-t  |
| 12c   | OSM2                 |                      | osa-express-6s-1000base-t  |
| 138   | CRYP02               |                      | crypto-express-6s          |
| 13c   | CRYP03               |                      | crypto-express-6s          |
| 7c1   | test_hip             | HS adapter test_hip  | hipersockets               |

## Partitions of the CPC

| Name  | Description    | Network Adapters   | Storage Adapters   | Crypto Adapters    |
|:----- |:-------------- |:------------------ |:------------------ |:------------------ |
| TEST1 | Test partition | 108, 110, 114, 128 | 100, 101, 104, 105 |                    |
| TEST2 | Test partition | 108, 128           | 100, 101, 104, 105 | 118, 11c, 138, 13c |
