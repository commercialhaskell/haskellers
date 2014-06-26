#Haskellers
Full source code for the haskellers.com website. Use as a base for your own Haskell/Yesod applications or as a sample application to help with learning Haskell/Yesod

### System Requirements
You must have Haskell, Yesod and Postgresql installed.

Instructions for installing Haskell and Yesod are available at http://www.yesodweb.com/page/quickstart

You will also need to install libicu-dev. This can be installed on debian based machines with:

```
apt-get install libicu-dev
```
    
### Installation
1. Download the source code to an appropriate folder. Just run:
    ```
    git clone https://github.com/snoyberg/haskellers.git`
    ```
Alternatively download the zip archive at https://github.com/snoyberg/haskellers/archive/master.zip and extract to a suitable folder.

2. cd to the haskellers directory created above.

3. Download and install local copies of all the libraries needed by haskellers.com. Just run:
    ```
    cabal sandbox init
    cabal install --only-dependencies --reorder-goals --max-backjumps=-1
    ```    
4. create a new postgresql database for the haskellers data. Just run:

    ```
    sudo su - postgres
    psql template1
    CREATE USER <username> WITH PASSWORD '<pwd>';
    CREATE DATABASE <dbname>;
    GRANT ALL PRIVILEGES ON DATABASE <dbname> TO <name>;
    \q
    ```
5. Rename `config/postgres-dummy.yml` to `config/postgres.yml` and edit it to reflect the choices you made in step 4.

6. Rename `SESCred-dummy.hs` to `SESCred.hs`. Replace the `fake-secret-key` and `fake-access-key` with random, unguessable strings. 

7. Start the haskellers application by running `yesod devel`.

