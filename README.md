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

3. Download and install local copies of all the libraries needed by haskellers.com. using [the Stack tool](https://github.com/commercialhaskell/stack/):
    ```
    stack install yesod-bin cabal-install --install-ghc && stack build
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
5. Copy `config/db/postgresql.yml.example` to `config/db/postgresql.yml`,
   and edit the latter to reflect the choices you made in step 4.

6. Copy `config/db/google-email.yaml.example` to `config/db/google-email.yaml`
   and `config/db/facebook.yaml.exaple` to `config/db/facebook.yaml`

7. Copy `config/db/aws.example` to `config/db/aws`, and in the latter
   replace `SOME-ACCESS-KEY` and `SOME-SECRET-KEY` with random,
   unguessable strings.

8. Start the haskellers application by running `stack exec -- yesod devel`.

