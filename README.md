# Trigger practice on Haskell servant 

Canonical example of the user REST API service 
in order to discover Haskell servant

## Check List

- [x] Implement base example
- [x] Add in memory cache using IORef
- [x] Separate concerns
- [x] Make handlers more visible
- [x] Create User
- [x] Delete User
- [ ] Add Auth
- [x] Add HTTPS
- [x] Use custom monad
- [ ] Add meaningful tests
- [ ] Generate IDs for management
- [ ] Makes status codes more meaningful 
- [ ] separate error management
- [ ] Add doc


## Usage

Make sure you have stack installed with version 1.9.+
Generate the certificates using the script `certs/gen_certs.sh` 

```bash
stack clean && stack setup && stack run
```

Provided that the `certs` directory contains the certs you generated you can
apply the following commands


##### Get all users
```bash
curl --cacert certs/cert.pem https://localhost:8080/users
```

##### Get users by name (waiting for ID)
```bash
curl --cacert certs/cert.pem https://localhost:8080/user/isaac
```

##### create a new user
```bash
curl --cacert certs/cert.pem -d '{"email":"globulon@gmail.com","registration_date":"1971-08-28", "name":"omd"}' -H "Content-Type: application/json" -X POST https://localhost:8080/user
```

##### delete a user
```bash
curl -cacert certs/cert.pem -v -X DELETE https://localhost:8080/user/andrea
```