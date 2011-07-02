#cutkey

cutkey is an Erlang app for generating RSA keys. It does so by calling the
OpenSSL key generation routines via a port driver. It may produce DSA keys as
well at some point.

Example usage:

```
$ erl -pa ebin -s cutkey -s crypto
Erlang R13B04 (erts-5.7.5) [source] [64-bit] [smp:2:2] [rq:2] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.7.5  (abort with ^G)
1> {ok,PrivateKey} = cutkey:rsa(2048,65537).
{ok,[<<0,0,0,3,1,0,1>>,
     <<0,0,1,1,0,208,50,129,227,74,131,95,200,199,112,10,97,8,
       188,19,224,176,23,181,248,...>>,
     <<0,0,1,0,115,79,166,21,7,78,32,29,10,41,9,246,152,105,
       226,240,141,87,116,151,...>>]}
2> PublicKey = lists:sublist(PrivateKey,2).
[<<0,0,0,3,1,0,1>>,
 <<0,0,1,1,0,208,50,129,227,74,131,95,200,199,112,10,97,8,
   188,19,224,176,23,181,248,89,222,...>>]
3> CipherText = crypto:rsa_private_encrypt(<<"superperfundo">>, PrivateKey, rsa_pkcs1_padding).
<<141,196,132,185,37,215,227,147,230,72,252,1,101,24,93,
  135,123,222,3,128,178,93,14,100,76,106,161,35,84,...>>
4> crypto:rsa_public_decrypt(CipherText, PublicKey, rsa_pkcs1_padding).
<<"superperfundo">>
```