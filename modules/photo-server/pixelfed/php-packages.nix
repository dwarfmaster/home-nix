{pixelfed-app, pixelfed-env, composerEnv, fetchurl, fetchgit ? null, fetchhg ? null, fetchsvn ? null, noDev ? false}:

let

  packages = {
    "alchemy/binary-driver" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "alchemy-binary-driver-e0615cdff315e6b4b05ada67906df6262a020d22";
        src = fetchurl {
          url = "https://api.github.com/repos/alchemy-fr/BinaryDriver/zipball/e0615cdff315e6b4b05ada67906df6262a020d22";
          sha256 = "1xfxillfyyvfhc3h4q5rsgip7d6x5xj959pchvx1mr18wl9yzpcv";
        };
      };
    };
    "asm89/stack-cors" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "asm89-stack-cors-9cb795bf30988e8c96dd3c40623c48a877bc6714";
        src = fetchurl {
          url = "https://api.github.com/repos/asm89/stack-cors/zipball/9cb795bf30988e8c96dd3c40623c48a877bc6714";
          sha256 = "1951g6pkyk9hh2vfssgfyiyw2wv6pvlq3j6xbids02yh5d7mi3xq";
        };
      };
    };
    "aws/aws-sdk-php" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "aws-aws-sdk-php-6c919bc226f7ff3fbcbce948f31e618066d02ad0";
        src = fetchurl {
          url = "https://api.github.com/repos/aws/aws-sdk-php/zipball/6c919bc226f7ff3fbcbce948f31e618066d02ad0";
          sha256 = "1wicn300x19v77dq11pwb6v91h8z11fqjrqrr8b9ncahyf5lg0aq";
        };
      };
    };
    "bacon/bacon-qr-code" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "bacon-bacon-qr-code-f73543ac4e1def05f1a70bcd1525c8a157a1ad09";
        src = fetchurl {
          url = "https://api.github.com/repos/Bacon/BaconQrCode/zipball/f73543ac4e1def05f1a70bcd1525c8a157a1ad09";
          sha256 = "1df22bfrc8q62qz8brrs8p2rmmv5gsaxdyjrd2ln6d6j7i4jkjpk";
        };
      };
    };
    "beyondcode/laravel-self-diagnosis" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "beyondcode-laravel-self-diagnosis-b9f235747a3b06cee3cffe152deabc37dc93cca4";
        src = fetchurl {
          url = "https://api.github.com/repos/beyondcode/laravel-self-diagnosis/zipball/b9f235747a3b06cee3cffe152deabc37dc93cca4";
          sha256 = "1wrrd943vq2vmsmr23kclkg9810b48adxha892li99zlczvmwgdx";
        };
      };
    };
    "brick/math" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "brick-math-e6f8e7d04346a95be89580f8c2c22d6c3fa65556";
        src = fetchurl {
          url = "https://api.github.com/repos/brick/math/zipball/e6f8e7d04346a95be89580f8c2c22d6c3fa65556";
          sha256 = "0x7mi1dhdrmnjcy41gx34115lpykd3zmmb0rk47s4d6n2vb6yppa";
        };
      };
    };
    "buzz/laravel-h-captcha" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "buzz-laravel-h-captcha-41a063bea0e204ae5b8afbafce981d4675dd8af7";
        src = fetchurl {
          url = "https://api.github.com/repos/thinhbuzz/laravel-h-captcha/zipball/41a063bea0e204ae5b8afbafce981d4675dd8af7";
          sha256 = "1iq14w26qf780q8jlxildjr67zjhq5fakg0ayv6xppj2jyixrl7b";
        };
      };
    };
    "composer/semver" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "composer-semver-647490bbcaf7fc4891c58f47b825eb99d19c377a";
        src = fetchurl {
          url = "https://api.github.com/repos/composer/semver/zipball/647490bbcaf7fc4891c58f47b825eb99d19c377a";
          sha256 = "16dx37b0b3qnla05h8l49hyg6khibd52i42lwqfyd91iysdpgz5r";
        };
      };
    };
    "dasprid/enum" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "dasprid-enum-5abf82f213618696dda8e3bf6f64dd042d8542b2";
        src = fetchurl {
          url = "https://api.github.com/repos/DASPRiD/Enum/zipball/5abf82f213618696dda8e3bf6f64dd042d8542b2";
          sha256 = "0rs7i1xiwhssy88s7bwnp5ri5fi2xy3fl7pw6l5k27xf2f1hv7q6";
        };
      };
    };
    "defuse/php-encryption" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "defuse-php-encryption-77880488b9954b7884c25555c2a0ea9e7053f9d2";
        src = fetchurl {
          url = "https://api.github.com/repos/defuse/php-encryption/zipball/77880488b9954b7884c25555c2a0ea9e7053f9d2";
          sha256 = "1lcvpg56nw72cxyh6sga7fx94qw9l0l1y78z7y7ny3hgdniwhihx";
        };
      };
    };
    "doctrine/cache" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "doctrine-cache-3bb5588cec00a0268829cc4a518490df6741af9d";
        src = fetchurl {
          url = "https://api.github.com/repos/doctrine/cache/zipball/3bb5588cec00a0268829cc4a518490df6741af9d";
          sha256 = "0r9fhv0y79ma7a5llmj1skycflnwbxyyrblkavjj6svld46li94q";
        };
      };
    };
    "doctrine/dbal" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "doctrine-dbal-8dd39d2ead4409ce652fd4f02621060f009ea5e4";
        src = fetchurl {
          url = "https://api.github.com/repos/doctrine/dbal/zipball/8dd39d2ead4409ce652fd4f02621060f009ea5e4";
          sha256 = "107k0qr3m34cjxy00yhdjmd8liqa8wg729zj4z2jifz26niiy8qs";
        };
      };
    };
    "doctrine/deprecations" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "doctrine-deprecations-9504165960a1f83cc1480e2be1dd0a0478561314";
        src = fetchurl {
          url = "https://api.github.com/repos/doctrine/deprecations/zipball/9504165960a1f83cc1480e2be1dd0a0478561314";
          sha256 = "04kpbzk5iw86imspkg7dgs54xx877k9b5q0dfg2h119mlfkvxil6";
        };
      };
    };
    "doctrine/event-manager" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "doctrine-event-manager-41370af6a30faa9dc0368c4a6814d596e81aba7f";
        src = fetchurl {
          url = "https://api.github.com/repos/doctrine/event-manager/zipball/41370af6a30faa9dc0368c4a6814d596e81aba7f";
          sha256 = "0pn2aiwl4fvv6fcwar9alng2yrqy8bzc58n4bkp6y2jnpw5gp4m8";
        };
      };
    };
    "doctrine/inflector" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "doctrine-inflector-9cf661f4eb38f7c881cac67c75ea9b00bf97b210";
        src = fetchurl {
          url = "https://api.github.com/repos/doctrine/inflector/zipball/9cf661f4eb38f7c881cac67c75ea9b00bf97b210";
          sha256 = "0gkaw5aqkdppd7cz1n46kdms0bv8kzbnpjh75jnhv98p9fik7f24";
        };
      };
    };
    "doctrine/lexer" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "doctrine-lexer-e864bbf5904cb8f5bb334f99209b48018522f042";
        src = fetchurl {
          url = "https://api.github.com/repos/doctrine/lexer/zipball/e864bbf5904cb8f5bb334f99209b48018522f042";
          sha256 = "11lg9fcy0crb8inklajhx3kyffdbx7xzdj8kwl21xsgq9nm9iwvv";
        };
      };
    };
    "dragonmantank/cron-expression" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "dragonmantank-cron-expression-7a8c6e56ab3ffcc538d05e8155bb42269abf1a0c";
        src = fetchurl {
          url = "https://api.github.com/repos/dragonmantank/cron-expression/zipball/7a8c6e56ab3ffcc538d05e8155bb42269abf1a0c";
          sha256 = "0pl9zrj9254qbwr7vyiilzhmb7bq2ss631iwvlq1mqky2bwinj2l";
        };
      };
    };
    "egulias/email-validator" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "egulias-email-validator-0dbf5d78455d4d6a41d186da50adc1122ec066f4";
        src = fetchurl {
          url = "https://api.github.com/repos/egulias/EmailValidator/zipball/0dbf5d78455d4d6a41d186da50adc1122ec066f4";
          sha256 = "00kwb8rhk1fq3a1i152xniipk3y907q1v5r3szqbkq5rz82dwbck";
        };
      };
    };
    "evenement/evenement" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "evenement-evenement-531bfb9d15f8aa57454f5f0285b18bec903b8fb7";
        src = fetchurl {
          url = "https://api.github.com/repos/igorw/evenement/zipball/531bfb9d15f8aa57454f5f0285b18bec903b8fb7";
          sha256 = "02mi1lrga41caa25whr6sj9hmmlfjp10l0d0fq8kc3d4483pm9rr";
        };
      };
    };
    "ezyang/htmlpurifier" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "ezyang-htmlpurifier-08e27c97e4c6ed02f37c5b2b20488046c8d90d75";
        src = fetchurl {
          url = "https://api.github.com/repos/ezyang/htmlpurifier/zipball/08e27c97e4c6ed02f37c5b2b20488046c8d90d75";
          sha256 = "0jna0hhd63w9bs39kxw4gid35jg9rg928lzk3rrcvalq9zv957fz";
        };
      };
    };
    "fideloper/proxy" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "fideloper-proxy-c073b2bd04d1c90e04dc1b787662b558dd65ade0";
        src = fetchurl {
          url = "https://api.github.com/repos/fideloper/TrustedProxy/zipball/c073b2bd04d1c90e04dc1b787662b558dd65ade0";
          sha256 = "05jzgjj4fy5p1smqj41b5qxj42zn0mnczvsaacni4fmq174mz4gy";
        };
      };
    };
    "firebase/php-jwt" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "firebase-php-jwt-d2113d9b2e0e349796e72d2a63cf9319100382d2";
        src = fetchurl {
          url = "https://api.github.com/repos/firebase/php-jwt/zipball/d2113d9b2e0e349796e72d2a63cf9319100382d2";
          sha256 = "1h686jkx4x7f2l84r932ydhfdgnh29hwy96nhy7p7n4dl7xk2c73";
        };
      };
    };
    "fruitcake/laravel-cors" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "fruitcake-laravel-cors-a8ccedc7ca95189ead0e407c43b530dc17791d6a";
        src = fetchurl {
          url = "https://api.github.com/repos/fruitcake/laravel-cors/zipball/a8ccedc7ca95189ead0e407c43b530dc17791d6a";
          sha256 = "0m1nh2fh9w2332j6v8j9wl9lh10gi9b36a3c2fgxzr2xy89mk678";
        };
      };
    };
    "geerlingguy/ping" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "geerlingguy-ping-e0206326e23c99e3e8820e24705f8ca517adff93";
        src = fetchurl {
          url = "https://api.github.com/repos/geerlingguy/Ping/zipball/e0206326e23c99e3e8820e24705f8ca517adff93";
          sha256 = "0vl2s6as1r1kfdfx4q73bsj8fmnjlxz9r2w8f3gwxav1m97v5dvs";
        };
      };
    };
    "graham-campbell/result-type" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "graham-campbell-result-type-7e279d2cd5d7fbb156ce46daada972355cea27bb";
        src = fetchurl {
          url = "https://api.github.com/repos/GrahamCampbell/Result-Type/zipball/7e279d2cd5d7fbb156ce46daada972355cea27bb";
          sha256 = "0hvbv2svljw2hyshbby7wrh29nck98rpbhfl911gyb89i8mzx1zm";
        };
      };
    };
    "guzzlehttp/guzzle" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "guzzlehttp-guzzle-9d4290de1cfd701f38099ef7e183b64b4b7b0c5e";
        src = fetchurl {
          url = "https://api.github.com/repos/guzzle/guzzle/zipball/9d4290de1cfd701f38099ef7e183b64b4b7b0c5e";
          sha256 = "1dlrdpil0173cmx73ghy8iis2j0lk00dzv3n166d0riky21n8djb";
        };
      };
    };
    "guzzlehttp/promises" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "guzzlehttp-promises-8e7d04f1f6450fef59366c399cfad4b9383aa30d";
        src = fetchurl {
          url = "https://api.github.com/repos/guzzle/promises/zipball/8e7d04f1f6450fef59366c399cfad4b9383aa30d";
          sha256 = "158wd8nmvvl386c24lkr4jkwdhqpdj0dxdbjwh8iv6a2rgccjr2q";
        };
      };
    };
    "guzzlehttp/psr7" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "guzzlehttp-psr7-dc960a912984efb74d0a90222870c72c87f10c91";
        src = fetchurl {
          url = "https://api.github.com/repos/guzzle/psr7/zipball/dc960a912984efb74d0a90222870c72c87f10c91";
          sha256 = "06nc60wf8k6kcl89kprk04khsrrik5lnis615mx4a0m0pjn8bliz";
        };
      };
    };
    "intervention/image" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "intervention-image-abbf18d5ab8367f96b3205ca3c89fb2fa598c69e";
        src = fetchurl {
          url = "https://api.github.com/repos/Intervention/image/zipball/abbf18d5ab8367f96b3205ca3c89fb2fa598c69e";
          sha256 = "1msfpr9bip69bmhg23ka2f43phgb6dq5z604j5psjh3xd86r6c5d";
        };
      };
    };
    "jaybizzle/crawler-detect" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "jaybizzle-crawler-detect-78bf6792cbf9c569dc0bf2465481978fd2ed0de9";
        src = fetchurl {
          url = "https://api.github.com/repos/JayBizzle/Crawler-Detect/zipball/78bf6792cbf9c569dc0bf2465481978fd2ed0de9";
          sha256 = "0bc2bc8n87453nrg29csqzcxxy21li7f4qyzbfgmdmyphncbqwp1";
        };
      };
    };
    "jenssegers/agent" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "jenssegers-agent-daa11c43729510b3700bc34d414664966b03bffe";
        src = fetchurl {
          url = "https://api.github.com/repos/jenssegers/agent/zipball/daa11c43729510b3700bc34d414664966b03bffe";
          sha256 = "0f0wy69w9mdsajfgriwlnpqhqxp83q44p6ggcd6h1bi8ri3h0897";
        };
      };
    };
    "laravel/framework" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "laravel-framework-edc138060a13c9e5f15c005fad5f6a39b4ccf5fa";
        src = fetchurl {
          url = "https://api.github.com/repos/laravel/framework/zipball/edc138060a13c9e5f15c005fad5f6a39b4ccf5fa";
          sha256 = "096k36jz1dwgm8q7w9cr6j22njd0jiirq9fmh6qh4jakzlfv57pk";
        };
      };
    };
    "laravel/helpers" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "laravel-helpers-febb10d8daaf86123825de2cb87f789a3371f0ac";
        src = fetchurl {
          url = "https://api.github.com/repos/laravel/helpers/zipball/febb10d8daaf86123825de2cb87f789a3371f0ac";
          sha256 = "1axbawm5hamfqvs5a6n4bbjc2fs5q3zwpsf7xrvqirxc4rgrdbgw";
        };
      };
    };
    "laravel/horizon" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "laravel-horizon-50dc53b105ec612cf1a87e89fffd21cc0a43003c";
        src = fetchurl {
          url = "https://api.github.com/repos/laravel/horizon/zipball/50dc53b105ec612cf1a87e89fffd21cc0a43003c";
          sha256 = "1gfsaygvafs61yhmv3jscj26n4brs5dyxl8f69gq547wlmxwdqsd";
        };
      };
    };
    "laravel/passport" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "laravel-passport-a5e4471dd99b7638ab5ca3ecab6cd87cf37eb410";
        src = fetchurl {
          url = "https://api.github.com/repos/laravel/passport/zipball/a5e4471dd99b7638ab5ca3ecab6cd87cf37eb410";
          sha256 = "18mlh7cvmmrrvfsfa7ramwmyqwn4n0y5a9wm9mb3msd4hvlgzvdn";
        };
      };
    };
    "laravel/tinker" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "laravel-tinker-04ad32c1a3328081097a181875733fa51f402083";
        src = fetchurl {
          url = "https://api.github.com/repos/laravel/tinker/zipball/04ad32c1a3328081097a181875733fa51f402083";
          sha256 = "1h4847a5rq2qdyszvjx6bqw5c0xi2m3pn9x7cqnq7jz7fkzpi5f9";
        };
      };
    };
    "laravel/ui" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "laravel-ui-2ccaa3b821ea8ac7e05393b946d0578bdb46099b";
        src = fetchurl {
          url = "https://api.github.com/repos/laravel/ui/zipball/2ccaa3b821ea8ac7e05393b946d0578bdb46099b";
          sha256 = "1mh1ax6s6cxg0nvvdk1irm75vrapbfcal54yrsjfcjk1x9zhjc4v";
        };
      };
    };
    "lcobucci/jwt" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "lcobucci-jwt-511629a54465e89a31d3d7e4cf0935feab8b14c1";
        src = fetchurl {
          url = "https://api.github.com/repos/lcobucci/jwt/zipball/511629a54465e89a31d3d7e4cf0935feab8b14c1";
          sha256 = "1zgrk6f3daa8flwjlljm7jg7cnnxf8z14h804fljklw7v1w3id31";
        };
      };
    };
    "league/commonmark" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "league-commonmark-c3c8b7217c52572fb42aaf84211abccf75a151b2";
        src = fetchurl {
          url = "https://api.github.com/repos/thephpleague/commonmark/zipball/c3c8b7217c52572fb42aaf84211abccf75a151b2";
          sha256 = "14r99bx2z1cbcrdp9ynjsrgyliqj0x033ay65zd28c30pfabaiyd";
        };
      };
    };
    "league/event" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "league-event-d2cc124cf9a3fab2bb4ff963307f60361ce4d119";
        src = fetchurl {
          url = "https://api.github.com/repos/thephpleague/event/zipball/d2cc124cf9a3fab2bb4ff963307f60361ce4d119";
          sha256 = "1fc8aj0mpbrnh3b93gn8pypix28nf2gfvi403kfl7ibh5iz6ds5l";
        };
      };
    };
    "league/flysystem" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "league-flysystem-f3ad69181b8afed2c9edf7be5a2918144ff4ea32";
        src = fetchurl {
          url = "https://api.github.com/repos/thephpleague/flysystem/zipball/f3ad69181b8afed2c9edf7be5a2918144ff4ea32";
          sha256 = "0s4sx4j7c16qkk7m6k2r4ajfjidlv15z18ybxhfmmz4jb4wsmv94";
        };
      };
    };
    "league/flysystem-aws-s3-v3" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "league-flysystem-aws-s3-v3-4e25cc0582a36a786c31115e419c6e40498f6972";
        src = fetchurl {
          url = "https://api.github.com/repos/thephpleague/flysystem-aws-s3-v3/zipball/4e25cc0582a36a786c31115e419c6e40498f6972";
          sha256 = "1q2vkgyaz7h6z3q0z3v3l5rsvhv4xc45prgzr214cgm656i2h1ab";
        };
      };
    };
    "league/flysystem-cached-adapter" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "league-flysystem-cached-adapter-d1925efb2207ac4be3ad0c40b8277175f99ffaff";
        src = fetchurl {
          url = "https://api.github.com/repos/thephpleague/flysystem-cached-adapter/zipball/d1925efb2207ac4be3ad0c40b8277175f99ffaff";
          sha256 = "1gvp89cl27ypcy4h0qjm04dc5k77jfm95m4paasglzfsi6g40i71";
        };
      };
    };
    "league/iso3166" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "league-iso3166-aed3b32fc293afdf2c6c6a322c2408eb5d20804a";
        src = fetchurl {
          url = "https://api.github.com/repos/thephpleague/iso3166/zipball/aed3b32fc293afdf2c6c6a322c2408eb5d20804a";
          sha256 = "0g2rp8zplazzhn23cs1x4qrz7znjf0cww0bq5gl1hcjzh7p5025i";
        };
      };
    };
    "league/mime-type-detection" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "league-mime-type-detection-3b9dff8aaf7323590c1d2e443db701eb1f9aa0d3";
        src = fetchurl {
          url = "https://api.github.com/repos/thephpleague/mime-type-detection/zipball/3b9dff8aaf7323590c1d2e443db701eb1f9aa0d3";
          sha256 = "0pmq486v2nf6672y2z53cyb3mfrxcc8n7z2ilpzz9zkkf2yb990j";
        };
      };
    };
    "league/oauth2-server" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "league-oauth2-server-97dbc97b3b1bc4e613b70cb5e0e07d4b2d9372cc";
        src = fetchurl {
          url = "https://api.github.com/repos/thephpleague/oauth2-server/zipball/97dbc97b3b1bc4e613b70cb5e0e07d4b2d9372cc";
          sha256 = "0am2j9x264ncga95fi622kn7mm89imh2cqxmskgi64bqlgxcp7sp";
        };
      };
    };
    "mobiledetect/mobiledetectlib" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "mobiledetect-mobiledetectlib-9841e3c46f5bd0739b53aed8ac677fa712943df7";
        src = fetchurl {
          url = "https://api.github.com/repos/serbanghita/Mobile-Detect/zipball/9841e3c46f5bd0739b53aed8ac677fa712943df7";
          sha256 = "02r9n3kj5rzkakj1fkrkhgpram9xyq2vl9paxza4k117fj51azcw";
        };
      };
    };
    "monolog/monolog" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "monolog-monolog-1cb1cde8e8dd0f70cc0fe51354a59acad9302084";
        src = fetchurl {
          url = "https://api.github.com/repos/Seldaek/monolog/zipball/1cb1cde8e8dd0f70cc0fe51354a59acad9302084";
          sha256 = "1gymdiymwrjw25fjqapq3jlmf6wnp1h26ms74sckd70d53c4m52k";
        };
      };
    };
    "mtdowling/jmespath.php" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "mtdowling-jmespath.php-9b87907a81b87bc76d19a7fb2d61e61486ee9edb";
        src = fetchurl {
          url = "https://api.github.com/repos/jmespath/jmespath.php/zipball/9b87907a81b87bc76d19a7fb2d61e61486ee9edb";
          sha256 = "1ig3gi6f8gisagcn876598ps48s86s6m0c82diyksylarg3yn0yd";
        };
      };
    };
    "nesbot/carbon" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "nesbot-carbon-93d9db91c0235c486875d22f1e08b50bdf3e6eee";
        src = fetchurl {
          url = "https://api.github.com/repos/briannesbitt/Carbon/zipball/93d9db91c0235c486875d22f1e08b50bdf3e6eee";
          sha256 = "1xk4bg9imbsm8481mc49111cimanrxswqqkj3bqs4ja8bam6a51z";
        };
      };
    };
    "neutron/temporary-filesystem" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "neutron-temporary-filesystem-60e79adfd16f42f4b888e351ad49f9dcb959e3c2";
        src = fetchurl {
          url = "https://api.github.com/repos/romainneutron/Temporary-Filesystem/zipball/60e79adfd16f42f4b888e351ad49f9dcb959e3c2";
          sha256 = "1fx9l8dvlcy0yv53k32hi2lhidc6wllw8r84hy75hikllakx97ki";
        };
      };
    };
    "nikic/php-parser" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "nikic-php-parser-4432ba399e47c66624bc73c8c0f811e5c109576f";
        src = fetchurl {
          url = "https://api.github.com/repos/nikic/PHP-Parser/zipball/4432ba399e47c66624bc73c8c0f811e5c109576f";
          sha256 = "0372c09xdgdr9dhd9m7sblxyqxk9xdk2r9s0i13ja3ascsz3zvpd";
        };
      };
    };
    "nyholm/psr7" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "nyholm-psr7-23ae1f00fbc6a886cbe3062ca682391b9cc7c37b";
        src = fetchurl {
          url = "https://api.github.com/repos/Nyholm/psr7/zipball/23ae1f00fbc6a886cbe3062ca682391b9cc7c37b";
          sha256 = "1lww7xhzlxsp0i65sg2xrz3n9m0nrz1ckgdcqdd75lxlw9s0c8vn";
        };
      };
    };
    "opis/closure" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "opis-closure-06e2ebd25f2869e54a306dda991f7db58066f7f6";
        src = fetchurl {
          url = "https://api.github.com/repos/opis/closure/zipball/06e2ebd25f2869e54a306dda991f7db58066f7f6";
          sha256 = "0fpa1w0rmwywj67jgaldmw563p7gycahs8gpkpjvrra9zhhj4yyc";
        };
      };
    };
    "paragonie/constant_time_encoding" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "paragonie-constant_time_encoding-f34c2b11eb9d2c9318e13540a1dbc2a3afbd939c";
        src = fetchurl {
          url = "https://api.github.com/repos/paragonie/constant_time_encoding/zipball/f34c2b11eb9d2c9318e13540a1dbc2a3afbd939c";
          sha256 = "1r1xj3j7s5mskw5gh3ars4dfhvcn7d252gdqgpif80026kj5fvrp";
        };
      };
    };
    "paragonie/random_compat" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "paragonie-random_compat-996434e5492cb4c3edcb9168db6fbb1359ef965a";
        src = fetchurl {
          url = "https://api.github.com/repos/paragonie/random_compat/zipball/996434e5492cb4c3edcb9168db6fbb1359ef965a";
          sha256 = "0ky7lal59dihf969r1k3pb96ql8zzdc5062jdbg69j6rj0scgkyx";
        };
      };
    };
    "pbmedia/laravel-ffmpeg" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "pbmedia-laravel-ffmpeg-95b75f41edce12f513df12928b10b8f6949ffe56";
        src = fetchurl {
          url = "https://api.github.com/repos/protonemedia/laravel-ffmpeg/zipball/95b75f41edce12f513df12928b10b8f6949ffe56";
          sha256 = "0nqr3ad5nx0lg6jnb9lihknqhixm4w275hq0ws5dwdval55dpb8v";
        };
      };
    };
    "php-ffmpeg/php-ffmpeg" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "php-ffmpeg-php-ffmpeg-edc0a7729d8818ed883e77b3d26ceb6d49ec41de";
        src = fetchurl {
          url = "https://api.github.com/repos/PHP-FFMpeg/PHP-FFMpeg/zipball/edc0a7729d8818ed883e77b3d26ceb6d49ec41de";
          sha256 = "0ilhmlyxy6jdmqw7vmdbmf0z19xrfp61wm008snrxml0lg9wms49";
        };
      };
    };
    "php-http/message-factory" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "php-http-message-factory-a478cb11f66a6ac48d8954216cfed9aa06a501a1";
        src = fetchurl {
          url = "https://api.github.com/repos/php-http/message-factory/zipball/a478cb11f66a6ac48d8954216cfed9aa06a501a1";
          sha256 = "13drpc83bq332hz0b97whibkm7jpk56msq4yppw9nmrchzwgy7cs";
        };
      };
    };
    "phpoption/phpoption" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpoption-phpoption-994ecccd8f3283ecf5ac33254543eb0ac946d525";
        src = fetchurl {
          url = "https://api.github.com/repos/schmittjoh/php-option/zipball/994ecccd8f3283ecf5ac33254543eb0ac946d525";
          sha256 = "1snrnfvqhnr5z9llf8kbqk9l97gfyp8gghmhi1ng8qx5xzv1anr7";
        };
      };
    };
    "phpseclib/phpseclib" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpseclib-phpseclib-f5c4c19880d45d0be3e7d24ae8ac434844a898cd";
        src = fetchurl {
          url = "https://api.github.com/repos/phpseclib/phpseclib/zipball/f5c4c19880d45d0be3e7d24ae8ac434844a898cd";
          sha256 = "0n3hhhvwb6zlf4vqdnldjpw6d6fhs3wj3fygq6zrwsqlvvwkrj43";
        };
      };
    };
    "pixelfed/fractal" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "pixelfed-fractal-faff10c9f3e3300b1571ef41926f933a9cce4782";
        src = fetchurl {
          url = "https://api.github.com/repos/pixelfed/fractal/zipball/faff10c9f3e3300b1571ef41926f933a9cce4782";
          sha256 = "054zbf39ghxk7xydphikxpgkw7hivxmjqzwpcqnpw2vpn3giwfay";
        };
      };
    };
    "pixelfed/laravel-snowflake" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "pixelfed-laravel-snowflake-69255870dcbf949feac889dfc09180a6fef77f6d";
        src = fetchurl {
          url = "https://api.github.com/repos/pixelfed/laravel-snowflake/zipball/69255870dcbf949feac889dfc09180a6fef77f6d";
          sha256 = "1wsgl9066z1zs751msqn5ydc6mz9m22wchy56qk9igjnjmk6g2pj";
        };
      };
    };
    "pixelfed/zttp" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "pixelfed-zttp-9a95a42716eb3e71a0a88411805737965bb77c05";
        src = fetchurl {
          url = "https://api.github.com/repos/pixelfed/zttp/zipball/9a95a42716eb3e71a0a88411805737965bb77c05";
          sha256 = "1069qxaz5338sqm1kziwr46czjh55vjvrlzmw8hzsf0pz8ykywln";
        };
      };
    };
    "pragmarx/google2fa" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "pragmarx-google2fa-26c4c5cf30a2844ba121760fd7301f8ad240100b";
        src = fetchurl {
          url = "https://api.github.com/repos/antonioribeiro/google2fa/zipball/26c4c5cf30a2844ba121760fd7301f8ad240100b";
          sha256 = "1jmc7s3hbczvb0h4kfmya67l969nfww3lmc4slvzsz0zd769434h";
        };
      };
    };
    "predis/predis" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "predis-predis-b240daa106d4e02f0c5b7079b41e31ddf66fddf8";
        src = fetchurl {
          url = "https://api.github.com/repos/predis/predis/zipball/b240daa106d4e02f0c5b7079b41e31ddf66fddf8";
          sha256 = "0wbsmq5c449vwfvsriyjwqaq5sjf9kw2chr4f2xlh3vqc4kw720j";
        };
      };
    };
    "psr/cache" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "psr-cache-d11b50ad223250cf17b86e38383413f5a6764bf8";
        src = fetchurl {
          url = "https://api.github.com/repos/php-fig/cache/zipball/d11b50ad223250cf17b86e38383413f5a6764bf8";
          sha256 = "06i2k3dx3b4lgn9a4v1dlgv8l9wcl4kl7vzhh63lbji0q96hv8qz";
        };
      };
    };
    "psr/container" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "psr-container-8622567409010282b7aeebe4bb841fe98b58dcaf";
        src = fetchurl {
          url = "https://api.github.com/repos/php-fig/container/zipball/8622567409010282b7aeebe4bb841fe98b58dcaf";
          sha256 = "0qfvyfp3mli776kb9zda5cpc8cazj3prk0bg0gm254kwxyfkfrwn";
        };
      };
    };
    "psr/event-dispatcher" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "psr-event-dispatcher-dbefd12671e8a14ec7f180cab83036ed26714bb0";
        src = fetchurl {
          url = "https://api.github.com/repos/php-fig/event-dispatcher/zipball/dbefd12671e8a14ec7f180cab83036ed26714bb0";
          sha256 = "05nicsd9lwl467bsv4sn44fjnnvqvzj1xqw2mmz9bac9zm66fsjd";
        };
      };
    };
    "psr/http-factory" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "psr-http-factory-12ac7fcd07e5b077433f5f2bee95b3a771bf61be";
        src = fetchurl {
          url = "https://api.github.com/repos/php-fig/http-factory/zipball/12ac7fcd07e5b077433f5f2bee95b3a771bf61be";
          sha256 = "0inbnqpc5bfhbbda9dwazsrw9xscfnc8rdx82q1qm3r446mc1vds";
        };
      };
    };
    "psr/http-message" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "psr-http-message-f6561bf28d520154e4b0ec72be95418abe6d9363";
        src = fetchurl {
          url = "https://api.github.com/repos/php-fig/http-message/zipball/f6561bf28d520154e4b0ec72be95418abe6d9363";
          sha256 = "195dd67hva9bmr52iadr4kyp2gw2f5l51lplfiay2pv6l9y4cf45";
        };
      };
    };
    "psr/log" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "psr-log-d49695b909c3b7628b6289db5479a1c204601f11";
        src = fetchurl {
          url = "https://api.github.com/repos/php-fig/log/zipball/d49695b909c3b7628b6289db5479a1c204601f11";
          sha256 = "0sb0mq30dvmzdgsnqvw3xh4fb4bqjncx72kf8n622f94dd48amln";
        };
      };
    };
    "psr/simple-cache" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "psr-simple-cache-408d5eafb83c57f6365a3ca330ff23aa4a5fa39b";
        src = fetchurl {
          url = "https://api.github.com/repos/php-fig/simple-cache/zipball/408d5eafb83c57f6365a3ca330ff23aa4a5fa39b";
          sha256 = "1djgzclkamjxi9jy4m9ggfzgq1vqxaga2ip7l3cj88p7rwkzjxgw";
        };
      };
    };
    "psy/psysh" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "psy-psysh-e4573f47750dd6c92dca5aee543fa77513cbd8d3";
        src = fetchurl {
          url = "https://api.github.com/repos/bobthecow/psysh/zipball/e4573f47750dd6c92dca5aee543fa77513cbd8d3";
          sha256 = "1pzw57gild4h66nfkvlcbz43ralypcjr9dgvwj6rs2gl72rfiwnk";
        };
      };
    };
    "ralouphie/getallheaders" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "ralouphie-getallheaders-120b605dfeb996808c31b6477290a714d356e822";
        src = fetchurl {
          url = "https://api.github.com/repos/ralouphie/getallheaders/zipball/120b605dfeb996808c31b6477290a714d356e822";
          sha256 = "1bv7ndkkankrqlr2b4kw7qp3fl0dxi6bp26bnim6dnlhavd6a0gg";
        };
      };
    };
    "ramsey/collection" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "ramsey-collection-28a5c4ab2f5111db6a60b2b4ec84057e0f43b9c1";
        src = fetchurl {
          url = "https://api.github.com/repos/ramsey/collection/zipball/28a5c4ab2f5111db6a60b2b4ec84057e0f43b9c1";
          sha256 = "18ka3y51a21bf7mv3hxxxnn1dj1mn3vg8y1i3j3ajsfi49xl6r03";
        };
      };
    };
    "ramsey/uuid" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "ramsey-uuid-cd4032040a750077205918c86049aa0f43d22947";
        src = fetchurl {
          url = "https://api.github.com/repos/ramsey/uuid/zipball/cd4032040a750077205918c86049aa0f43d22947";
          sha256 = "00hnl12crjs7kh67jhhjg157pma4ka5c5rpz46sdx8m207vhylzq";
        };
      };
    };
    "spatie/db-dumper" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "spatie-db-dumper-05e5955fb882008a8947c5a45146d86cfafa10d1";
        src = fetchurl {
          url = "https://api.github.com/repos/spatie/db-dumper/zipball/05e5955fb882008a8947c5a45146d86cfafa10d1";
          sha256 = "0g0scxq259qn1maxa61qh3cl5a88778qgx27dgbxr9p8kszivlsg";
        };
      };
    };
    "spatie/image-optimizer" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "spatie-image-optimizer-c22202fdd57856ed18a79cfab522653291a6e96a";
        src = fetchurl {
          url = "https://api.github.com/repos/spatie/image-optimizer/zipball/c22202fdd57856ed18a79cfab522653291a6e96a";
          sha256 = "0f4qyaphskxdb51x9aap9z5jj3lgz7wi4yc3wf29cxl9ssb3c538";
        };
      };
    };
    "spatie/laravel-backup" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "spatie-laravel-backup-6b2229a07d92c2bb146ad9c5223fc32e9d74830c";
        src = fetchurl {
          url = "https://api.github.com/repos/spatie/laravel-backup/zipball/6b2229a07d92c2bb146ad9c5223fc32e9d74830c";
          sha256 = "1k289cnlw5z5nph67qimbbddnqalq0v75y5flmq3l18p3v1sy6f6";
        };
      };
    };
    "spatie/laravel-image-optimizer" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "spatie-laravel-image-optimizer-c39e9ea77dee6b6eddfc26800adb1aa06a624294";
        src = fetchurl {
          url = "https://api.github.com/repos/spatie/laravel-image-optimizer/zipball/c39e9ea77dee6b6eddfc26800adb1aa06a624294";
          sha256 = "1z67ycij8mrcp8prl9iib1dmw9s2bin0xr6jqh5sgmybgkjqsd45";
        };
      };
    };
    "spatie/temporary-directory" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "spatie-temporary-directory-f517729b3793bca58f847c5fd383ec16f03ffec6";
        src = fetchurl {
          url = "https://api.github.com/repos/spatie/temporary-directory/zipball/f517729b3793bca58f847c5fd383ec16f03ffec6";
          sha256 = "1pn6l9c86yigpzn83ajpq2wiy8ds0rlxmiq0iwby14cijc98ma3m";
        };
      };
    };
    "stevebauman/purify" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "stevebauman-purify-08e8830f0ab9d302f8d76d4f5854910b24bacbb3";
        src = fetchurl {
          url = "https://api.github.com/repos/stevebauman/purify/zipball/08e8830f0ab9d302f8d76d4f5854910b24bacbb3";
          sha256 = "0r59533lb1yb61pchghv07b5bkda700pkxc23y4nbhfzqasz2160";
        };
      };
    };
    "swiftmailer/swiftmailer" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "swiftmailer-swiftmailer-15f7faf8508e04471f666633addacf54c0ab5933";
        src = fetchurl {
          url = "https://api.github.com/repos/swiftmailer/swiftmailer/zipball/15f7faf8508e04471f666633addacf54c0ab5933";
          sha256 = "1xiisdaxlmkzi16szh7lm3ay9vr9pdz0q2ah7vqaqrm2b4mwd90g";
        };
      };
    };
    "symfony/console" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-console-649730483885ff2ca99ca0560ef0e5f6b03f2ac1";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/console/zipball/649730483885ff2ca99ca0560ef0e5f6b03f2ac1";
          sha256 = "029c1a5hg6d8akflinl5k0hpvfaqmj9plbax9ppsjgxj9jba8aid";
        };
      };
    };
    "symfony/css-selector" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-css-selector-fcd0b29a7a0b1bb5bfbedc6231583d77fea04814";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/css-selector/zipball/fcd0b29a7a0b1bb5bfbedc6231583d77fea04814";
          sha256 = "0wdq2bfz3kl5b36316a114asl8mpcglksbvhnfbvn4mbbqhinv68";
        };
      };
    };
    "symfony/deprecation-contracts" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-deprecation-contracts-5f38c8804a9e97d23e0c8d63341088cd8a22d627";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/deprecation-contracts/zipball/5f38c8804a9e97d23e0c8d63341088cd8a22d627";
          sha256 = "11k6a8v9b6p0j788fgykq6s55baba29lg37fwvmn4igxxkfwmbp3";
        };
      };
    };
    "symfony/error-handler" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-error-handler-0e6768b8c0dcef26df087df2bbbaa143867a59b2";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/error-handler/zipball/0e6768b8c0dcef26df087df2bbbaa143867a59b2";
          sha256 = "0gs1mz6xzl0h4cc7wllk36dpm37cr8wczwvwn6sgl17pdbvrmncs";
        };
      };
    };
    "symfony/event-dispatcher" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-event-dispatcher-67a5f354afa8e2f231081b3fa11a5912f933c3ce";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/event-dispatcher/zipball/67a5f354afa8e2f231081b3fa11a5912f933c3ce";
          sha256 = "0qnaby62l2sw6zg4m258jmhy2llqc1jh8yg6wyx6snjimm008r3q";
        };
      };
    };
    "symfony/event-dispatcher-contracts" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-event-dispatcher-contracts-69fee1ad2332a7cbab3aca13591953da9cdb7a11";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/event-dispatcher-contracts/zipball/69fee1ad2332a7cbab3aca13591953da9cdb7a11";
          sha256 = "1xajgmj8fnix4q1p93mhhiwvxspm8p4ksgzyyh31sj4xsp1c41x7";
        };
      };
    };
    "symfony/filesystem" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-filesystem-348116319d7fb7d1faa781d26a48922428013eb2";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/filesystem/zipball/348116319d7fb7d1faa781d26a48922428013eb2";
          sha256 = "0r5gab1zdhgqr9ix0wa2d80nm7a2qvga89gh2awy6sfcb5ndhp5c";
        };
      };
    };
    "symfony/finder" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-finder-0ae3f047bed4edff6fd35b26a9a6bfdc92c953c6";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/finder/zipball/0ae3f047bed4edff6fd35b26a9a6bfdc92c953c6";
          sha256 = "0g7ywxk5qax371g9bc051kvbv7rq2r75ikj9zrxjlnp4gkbinf6m";
        };
      };
    };
    "symfony/http-foundation" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-http-foundation-7b6dd714d95106b831aaa7f3c9c612ab886516bd";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/http-foundation/zipball/7b6dd714d95106b831aaa7f3c9c612ab886516bd";
          sha256 = "19yaklcyfxphixjyr2rsw65dh61h0w1vsbdfy151dfqvxxk14vhx";
        };
      };
    };
    "symfony/http-kernel" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-http-kernel-3e32676e6cb5d2081c91a56783471ff8a7f7110b";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/http-kernel/zipball/3e32676e6cb5d2081c91a56783471ff8a7f7110b";
          sha256 = "0g9z0ww6anvai0zinscmz3gf4zxsby4id8nwiqm3p20qizi2dbb0";
        };
      };
    };
    "symfony/mime" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-mime-47dd7912152b82d0d4c8d9040dbc93d6232d472a";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/mime/zipball/47dd7912152b82d0d4c8d9040dbc93d6232d472a";
          sha256 = "1xyk50qrd5hzpwp795adbc56pqxljy3yalsd7p2bfyhd1y5ihwdv";
        };
      };
    };
    "symfony/polyfill-ctype" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-polyfill-ctype-46cd95797e9df938fdd2b03693b5fca5e64b01ce";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/polyfill-ctype/zipball/46cd95797e9df938fdd2b03693b5fca5e64b01ce";
          sha256 = "0z4iiznxxs4r72xs4irqqb6c0wnwpwf0hklwn2imls67haq330zn";
        };
      };
    };
    "symfony/polyfill-iconv" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-polyfill-iconv-63b5bb7db83e5673936d6e3b8b3e022ff6474933";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/polyfill-iconv/zipball/63b5bb7db83e5673936d6e3b8b3e022ff6474933";
          sha256 = "1jyjsjprsgb3r6cbc4x1wg1q1zqakqm8a62ah5lppxnjgq1sgjc5";
        };
      };
    };
    "symfony/polyfill-intl-grapheme" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-polyfill-intl-grapheme-24b72c6baa32c746a4d0840147c9715e42bb68ab";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/polyfill-intl-grapheme/zipball/24b72c6baa32c746a4d0840147c9715e42bb68ab";
          sha256 = "1ddgvsr2k585mj2vr3z1q56kxdbb5fin7y0ixhb1791x36km77f3";
        };
      };
    };
    "symfony/polyfill-intl-idn" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-polyfill-intl-idn-65bd267525e82759e7d8c4e8ceea44f398838e65";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/polyfill-intl-idn/zipball/65bd267525e82759e7d8c4e8ceea44f398838e65";
          sha256 = "1cx2cjx0vzni297l7avd3cb1q4c8d2hylkvdqcjlpxjqdimn4jkn";
        };
      };
    };
    "symfony/polyfill-intl-normalizer" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-polyfill-intl-normalizer-8590a5f561694770bdcd3f9b5c69dde6945028e8";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/polyfill-intl-normalizer/zipball/8590a5f561694770bdcd3f9b5c69dde6945028e8";
          sha256 = "1c60xin00q0d2gbyaiglxppn5hqwki616v5chzwyhlhf6aplwsh3";
        };
      };
    };
    "symfony/polyfill-mbstring" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-polyfill-mbstring-2df51500adbaebdc4c38dea4c89a2e131c45c8a1";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/polyfill-mbstring/zipball/2df51500adbaebdc4c38dea4c89a2e131c45c8a1";
          sha256 = "1fbi13p4a6nn01ix3gcj966kq6z8qx03li4vbjylsr9ac2mgnmnn";
        };
      };
    };
    "symfony/polyfill-php72" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-polyfill-php72-9a142215a36a3888e30d0a9eeea9766764e96976";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/polyfill-php72/zipball/9a142215a36a3888e30d0a9eeea9766764e96976";
          sha256 = "06ipbcvrxjzgvraf2z9fwgy0bzvzjvs5z1j67grg1gb15x3d428b";
        };
      };
    };
    "symfony/polyfill-php73" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-polyfill-php73-fba8933c384d6476ab14fb7b8526e5287ca7e010";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/polyfill-php73/zipball/fba8933c384d6476ab14fb7b8526e5287ca7e010";
          sha256 = "0fc1d60iw8iar2zcvkzwdvx0whkbw8p6ll0cry39nbkklzw85n1h";
        };
      };
    };
    "symfony/polyfill-php80" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-polyfill-php80-eca0bf41ed421bed1b57c4958bab16aa86b757d0";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/polyfill-php80/zipball/eca0bf41ed421bed1b57c4958bab16aa86b757d0";
          sha256 = "1y5kc4vqh920wyjdlgxp23b958g5i9mw10mhbr30vf8j20vf1gra";
        };
      };
    };
    "symfony/process" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-process-714b47f9196de61a196d86c4bad5f09201b307df";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/process/zipball/714b47f9196de61a196d86c4bad5f09201b307df";
          sha256 = "0r81fm2irx0vzwm9g5gj6lkz8adxvzv3d5ydxvn6ndbaznxck0cn";
        };
      };
    };
    "symfony/psr-http-message-bridge" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-psr-http-message-bridge-81db2d4ae86e9f0049828d9343a72b9523884e5d";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/psr-http-message-bridge/zipball/81db2d4ae86e9f0049828d9343a72b9523884e5d";
          sha256 = "0np4vrz4caslj83q7xabp6y8i15j2k5bm0y317r80zx9iyhpx6wm";
        };
      };
    };
    "symfony/routing" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-routing-368e81376a8e049c37cb80ae87dbfbf411279199";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/routing/zipball/368e81376a8e049c37cb80ae87dbfbf411279199";
          sha256 = "1xi6amczk9l5gdvzzgcagm7qyscbx7p24ppcdymbv2kfk6six9wf";
        };
      };
    };
    "symfony/service-contracts" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-service-contracts-f040a30e04b57fbcc9c6cbcf4dbaa96bd318b9bb";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/service-contracts/zipball/f040a30e04b57fbcc9c6cbcf4dbaa96bd318b9bb";
          sha256 = "1i573rmajc33a9nrgwgc4k3svg29yp9xv17gp133rd1i705hwv1y";
        };
      };
    };
    "symfony/string" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-string-0732e97e41c0a590f77e231afc16a327375d50b0";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/string/zipball/0732e97e41c0a590f77e231afc16a327375d50b0";
          sha256 = "0yv8wjv44qdmld9qn1ll1icv4xgyp6kgv4x0zqh20kzrsvr85nrg";
        };
      };
    };
    "symfony/translation" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-translation-7e2603bcc598e14804c4d2359d8dc4ee3c40391b";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/translation/zipball/7e2603bcc598e14804c4d2359d8dc4ee3c40391b";
          sha256 = "0kjdnlpzys01cwyj0q88yz0wyh8nay6flkr989p7s4g3qx1bdk5n";
        };
      };
    };
    "symfony/translation-contracts" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-translation-contracts-95c812666f3e91db75385749fe219c5e494c7f95";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/translation-contracts/zipball/95c812666f3e91db75385749fe219c5e494c7f95";
          sha256 = "073l1pbmwbkaviwwjq9ypb1w7dk366nn2vn1vancbal0zqk0zx7b";
        };
      };
    };
    "symfony/var-dumper" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "symfony-var-dumper-905a22c68b292ffb6f20d7636c36b220d1fba5ae";
        src = fetchurl {
          url = "https://api.github.com/repos/symfony/var-dumper/zipball/905a22c68b292ffb6f20d7636c36b220d1fba5ae";
          sha256 = "1np3j2rkvlnrvhma4zpbvbyifggm7iqliwz7n5zwsk9zgkjxgfb2";
        };
      };
    };
    "tightenco/collect" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "tightenco-collect-d7381736dca44ac17d0805a25191b094e5a22446";
        src = fetchurl {
          url = "https://api.github.com/repos/tighten/collect/zipball/d7381736dca44ac17d0805a25191b094e5a22446";
          sha256 = "0qzsr8q6x7ncwdpbp0w652gl260rwynxvrnsjvj86vjkbc4s0y8w";
        };
      };
    };
    "tijsverkoyen/css-to-inline-styles" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "tijsverkoyen-css-to-inline-styles-b43b05cf43c1b6d849478965062b6ef73e223bb5";
        src = fetchurl {
          url = "https://api.github.com/repos/tijsverkoyen/CssToInlineStyles/zipball/b43b05cf43c1b6d849478965062b6ef73e223bb5";
          sha256 = "0lc6jviz8faqxxs453dbqvfdmm6l2iczxla22v2r6xhakl58pf3w";
        };
      };
    };
    "vlucas/phpdotenv" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "vlucas-phpdotenv-b3eac5c7ac896e52deab4a99068e3f4ab12d9e56";
        src = fetchurl {
          url = "https://api.github.com/repos/vlucas/phpdotenv/zipball/b3eac5c7ac896e52deab4a99068e3f4ab12d9e56";
          sha256 = "1w8gylm0qwgwx2y3na9s2knpvc00yfhwf01p662l1cn9b3h33i11";
        };
      };
    };
    "voku/portable-ascii" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "voku-portable-ascii-80953678b19901e5165c56752d087fc11526017c";
        src = fetchurl {
          url = "https://api.github.com/repos/voku/portable-ascii/zipball/80953678b19901e5165c56752d087fc11526017c";
          sha256 = "112sz1jl55l3qm3041ijyzxy7qbv0sa6535hx6sp7nk2c76wjq0d";
        };
      };
    };
    "webmozart/assert" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "webmozart-assert-6964c76c7804814a842473e0c8fd15bab0f18e25";
        src = fetchurl {
          url = "https://api.github.com/repos/webmozarts/assert/zipball/6964c76c7804814a842473e0c8fd15bab0f18e25";
          sha256 = "17xqhb2wkwr7cgbl4xdjf7g1vkal17y79rpp6xjpf1xgl5vypc64";
        };
      };
    };
  };
  devPackages = {
    "brianium/paratest" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "brianium-paratest-268d5b2b4237c0abf76c4aa9633ad8580be01e1e";
        src = fetchurl {
          url = "https://api.github.com/repos/paratestphp/paratest/zipball/268d5b2b4237c0abf76c4aa9633ad8580be01e1e";
          sha256 = "1k6hs1qvqgkm92miwlsb41n7cyhcz64r2j2cwyq54rhyj46yjllb";
        };
      };
    };
    "doctrine/instantiator" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "doctrine-instantiator-d56bf6102915de5702778fe20f2de3b2fe570b5b";
        src = fetchurl {
          url = "https://api.github.com/repos/doctrine/instantiator/zipball/d56bf6102915de5702778fe20f2de3b2fe570b5b";
          sha256 = "04rihgfjv8alvvb92bnb5qpz8fvqvjwfrawcjw34pfnfx4jflcwh";
        };
      };
    };
    "facade/flare-client-php" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "facade-flare-client-php-47b639dc02bcfdfc4ebb83de703856fa01e35f5f";
        src = fetchurl {
          url = "https://api.github.com/repos/facade/flare-client-php/zipball/47b639dc02bcfdfc4ebb83de703856fa01e35f5f";
          sha256 = "1chpfxmnlpl98cg127i70ari7nb3w83l91l119hc2fhyjk0zrcy5";
        };
      };
    };
    "facade/ignition" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "facade-ignition-43688227bbf27c43bc1ad83af224f135b6ef0ff4";
        src = fetchurl {
          url = "https://api.github.com/repos/facade/ignition/zipball/43688227bbf27c43bc1ad83af224f135b6ef0ff4";
          sha256 = "0c16aqd0lbdwjj56wfns4bj4gs95gw30qyqvph9kfzsm5ww8azkc";
        };
      };
    };
    "facade/ignition-contracts" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "facade-ignition-contracts-3c921a1cdba35b68a7f0ccffc6dffc1995b18267";
        src = fetchurl {
          url = "https://api.github.com/repos/facade/ignition-contracts/zipball/3c921a1cdba35b68a7f0ccffc6dffc1995b18267";
          sha256 = "1nsjwd1k9q8qmfvh7m50rs42yxzxyq4f56r6dq205gwcmqsjb2j0";
        };
      };
    };
    "filp/whoops" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "filp-whoops-2edbc73a4687d9085c8f20f398eebade844e8424";
        src = fetchurl {
          url = "https://api.github.com/repos/filp/whoops/zipball/2edbc73a4687d9085c8f20f398eebade844e8424";
          sha256 = "1x79vnjdbjk9z2mix75ri56kyc5iwvjv3dyivncg6n3wd80nyfgg";
        };
      };
    };
    "fzaninotto/faker" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "fzaninotto-faker-848d8125239d7dbf8ab25cb7f054f1a630e68c2e";
        src = fetchurl {
          url = "https://api.github.com/repos/fzaninotto/Faker/zipball/848d8125239d7dbf8ab25cb7f054f1a630e68c2e";
          sha256 = "1nsbmkws5lwfm0nhy67q6awzwcb1qxgnqml6yfy3wfj7s62r6x09";
        };
      };
    };
    "hamcrest/hamcrest-php" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "hamcrest-hamcrest-php-8c3d0a3f6af734494ad8f6fbbee0ba92422859f3";
        src = fetchurl {
          url = "https://api.github.com/repos/hamcrest/hamcrest-php/zipball/8c3d0a3f6af734494ad8f6fbbee0ba92422859f3";
          sha256 = "1ixmmpplaf1z36f34d9f1342qjbcizvi5ddkjdli6jgrbla6a6hr";
        };
      };
    };
    "mockery/mockery" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "mockery-mockery-d1339f64479af1bee0e82a0413813fe5345a54ea";
        src = fetchurl {
          url = "https://api.github.com/repos/mockery/mockery/zipball/d1339f64479af1bee0e82a0413813fe5345a54ea";
          sha256 = "03ivhqdghsg5brgfq117ff5nj2s74d83rh34pzfqqpgca45h3w6d";
        };
      };
    };
    "myclabs/deep-copy" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "myclabs-deep-copy-776f831124e9c62e1a2c601ecc52e776d8bb7220";
        src = fetchurl {
          url = "https://api.github.com/repos/myclabs/DeepCopy/zipball/776f831124e9c62e1a2c601ecc52e776d8bb7220";
          sha256 = "181f3fsxs6s2wyy4y7qfk08qmlbvz1wn3mn3lqy42grsb8g8ym0k";
        };
      };
    };
    "nunomaduro/collision" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "nunomaduro-collision-b5cb36122f1c142c3c3ee20a0ae778439ef0244b";
        src = fetchurl {
          url = "https://api.github.com/repos/nunomaduro/collision/zipball/b5cb36122f1c142c3c3ee20a0ae778439ef0244b";
          sha256 = "0fv29ffl7wa0bwkl2n8qln5jdlpp9pvnigmdan7yismqflzp7jjl";
        };
      };
    };
    "phar-io/manifest" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phar-io-manifest-85265efd3af7ba3ca4b2a2c34dbfc5788dd29133";
        src = fetchurl {
          url = "https://api.github.com/repos/phar-io/manifest/zipball/85265efd3af7ba3ca4b2a2c34dbfc5788dd29133";
          sha256 = "13cqrx7iikx2aixszhxl55ql6hikblvbalix0kr05pbiccipg6fv";
        };
      };
    };
    "phar-io/version" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phar-io-version-bae7c545bef187884426f042434e561ab1ddb182";
        src = fetchurl {
          url = "https://api.github.com/repos/phar-io/version/zipball/bae7c545bef187884426f042434e561ab1ddb182";
          sha256 = "0hqmrihb4wv53rl3fg93wjldwrz79jyad5bv29ynbdklsirh7b2l";
        };
      };
    };
    "phpdocumentor/reflection-common" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpdocumentor-reflection-common-1d01c49d4ed62f25aa84a747ad35d5a16924662b";
        src = fetchurl {
          url = "https://api.github.com/repos/phpDocumentor/ReflectionCommon/zipball/1d01c49d4ed62f25aa84a747ad35d5a16924662b";
          sha256 = "1wx720a17i24471jf8z499dnkijzb4b8xra11kvw9g9hhzfadz1r";
        };
      };
    };
    "phpdocumentor/reflection-docblock" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpdocumentor-reflection-docblock-069a785b2141f5bcf49f3e353548dc1cce6df556";
        src = fetchurl {
          url = "https://api.github.com/repos/phpDocumentor/ReflectionDocBlock/zipball/069a785b2141f5bcf49f3e353548dc1cce6df556";
          sha256 = "0qid63bsfjmc3ka54f1ijl4a5zqwf7jmackjyjmbw3gxdnbi69il";
        };
      };
    };
    "phpdocumentor/type-resolver" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpdocumentor-type-resolver-6a467b8989322d92aa1c8bf2bebcc6e5c2ba55c0";
        src = fetchurl {
          url = "https://api.github.com/repos/phpDocumentor/TypeResolver/zipball/6a467b8989322d92aa1c8bf2bebcc6e5c2ba55c0";
          sha256 = "01g6mihq5wd1396njjb7ibcdfgk26ix1kmbjb6dlshzav0k3983h";
        };
      };
    };
    "phpspec/prophecy" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpspec-prophecy-be1996ed8adc35c3fd795488a653f4b518be70ea";
        src = fetchurl {
          url = "https://api.github.com/repos/phpspec/prophecy/zipball/be1996ed8adc35c3fd795488a653f4b518be70ea";
          sha256 = "167snpasy7499pbxpyx2bj607qa1vrg07xfpa30dlpbwi7f34dji";
        };
      };
    };
    "phpunit/php-code-coverage" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpunit-php-code-coverage-f6293e1b30a2354e8428e004689671b83871edde";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/php-code-coverage/zipball/f6293e1b30a2354e8428e004689671b83871edde";
          sha256 = "0q7az9h109jchlsgkzlnvzl90f39ifqp53k9bih85lbkaiz5w329";
        };
      };
    };
    "phpunit/php-file-iterator" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpunit-php-file-iterator-aa4be8575f26070b100fccb67faabb28f21f66f8";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/php-file-iterator/zipball/aa4be8575f26070b100fccb67faabb28f21f66f8";
          sha256 = "0vxnrzwb573ddmiw1sd77bdym6jiimwjhcz7yvmsr9wswkxh18l6";
        };
      };
    };
    "phpunit/php-invoker" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpunit-php-invoker-5a10147d0aaf65b58940a0b72f71c9ac0423cc67";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/php-invoker/zipball/5a10147d0aaf65b58940a0b72f71c9ac0423cc67";
          sha256 = "1vqnnjnw94mzm30n9n5p2bfgd3wd5jah92q6cj3gz1nf0qigr4fh";
        };
      };
    };
    "phpunit/php-text-template" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpunit-php-text-template-5da5f67fc95621df9ff4c4e5a84d6a8a2acf7c28";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/php-text-template/zipball/5da5f67fc95621df9ff4c4e5a84d6a8a2acf7c28";
          sha256 = "0ff87yzywizi6j2ps3w0nalpx16mfyw3imzn6gj9jjsfwc2bb8lq";
        };
      };
    };
    "phpunit/php-timer" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpunit-php-timer-5a63ce20ed1b5bf577850e2c4e87f4aa902afbd2";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/php-timer/zipball/5a63ce20ed1b5bf577850e2c4e87f4aa902afbd2";
          sha256 = "0g1g7yy4zk1bidyh165fsbqx5y8f1c8pxikvcahzlfsr9p2qxk6a";
        };
      };
    };
    "phpunit/phpunit" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "phpunit-phpunit-fb9b8333f14e3dce976a60ef6a7e05c7c7ed8bfb";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/phpunit/zipball/fb9b8333f14e3dce976a60ef6a7e05c7c7ed8bfb";
          sha256 = "0bvm4a3jgqgq2lvdp4k03h7bdd2z8yyh7603b9dlr8jzgk335qq3";
        };
      };
    };
    "sebastian/cli-parser" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "sebastian-cli-parser-442e7c7e687e42adc03470c7b668bc4b2402c0b2";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/cli-parser/zipball/442e7c7e687e42adc03470c7b668bc4b2402c0b2";
          sha256 = "074qzdq19k9x4svhq3nak5h348xska56v1sqnhk1aj0jnrx02h37";
        };
      };
    };
    "sebastian/code-unit" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "sebastian-code-unit-1fc9f64c0927627ef78ba436c9b17d967e68e120";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/code-unit/zipball/1fc9f64c0927627ef78ba436c9b17d967e68e120";
          sha256 = "04vlx050rrd54mxal7d93pz4119pas17w3gg5h532anfxjw8j7pm";
        };
      };
    };
    "sebastian/code-unit-reverse-lookup" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "sebastian-code-unit-reverse-lookup-ac91f01ccec49fb77bdc6fd1e548bc70f7faa3e5";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/code-unit-reverse-lookup/zipball/ac91f01ccec49fb77bdc6fd1e548bc70f7faa3e5";
          sha256 = "1h1jbzz3zak19qi4mab2yd0ddblpz7p000jfyxfwd2ds0gmrnsja";
        };
      };
    };
    "sebastian/comparator" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "sebastian-comparator-55f4261989e546dc112258c7a75935a81a7ce382";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/comparator/zipball/55f4261989e546dc112258c7a75935a81a7ce382";
          sha256 = "1d4bgf4m2x0kn3nw9hbb45asbx22lsp9vxl74rp1yl3sj2vk9sch";
        };
      };
    };
    "sebastian/complexity" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "sebastian-complexity-739b35e53379900cc9ac327b2147867b8b6efd88";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/complexity/zipball/739b35e53379900cc9ac327b2147867b8b6efd88";
          sha256 = "1y4yz8n8hszbhinf9ipx3pqyvgm7gz0krgyn19z0097yq3bbq8yf";
        };
      };
    };
    "sebastian/diff" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "sebastian-diff-3461e3fccc7cfdfc2720be910d3bd73c69be590d";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/diff/zipball/3461e3fccc7cfdfc2720be910d3bd73c69be590d";
          sha256 = "0967nl6cdnr0v0z83w4xy59agn60kfv8gb41aw3fpy1n2wpp62dj";
        };
      };
    };
    "sebastian/environment" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "sebastian-environment-388b6ced16caa751030f6a69e588299fa09200ac";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/environment/zipball/388b6ced16caa751030f6a69e588299fa09200ac";
          sha256 = "022vn8zss3sm7hg83kg3y0lmjw2ak6cy64b584nbsgxfhlmf6msd";
        };
      };
    };
    "sebastian/exporter" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "sebastian-exporter-d89cc98761b8cb5a1a235a6b703ae50d34080e65";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/exporter/zipball/d89cc98761b8cb5a1a235a6b703ae50d34080e65";
          sha256 = "1s8v0cbcjdb0wvwyh869y5f8d55mpjkr0f3gg2kvvxk3wh8nvvc7";
        };
      };
    };
    "sebastian/global-state" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "sebastian-global-state-23bd5951f7ff26f12d4e3242864df3e08dec4e49";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/global-state/zipball/23bd5951f7ff26f12d4e3242864df3e08dec4e49";
          sha256 = "0hrh5knc2g5i288kh9lkwmr6hb7yav5m8i21piz8pfh7kc75czjw";
        };
      };
    };
    "sebastian/lines-of-code" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "sebastian-lines-of-code-c1c2e997aa3146983ed888ad08b15470a2e22ecc";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/lines-of-code/zipball/c1c2e997aa3146983ed888ad08b15470a2e22ecc";
          sha256 = "0fay9s5cm16gbwr7qjihwrzxn7sikiwba0gvda16xng903argbk0";
        };
      };
    };
    "sebastian/object-enumerator" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "sebastian-object-enumerator-5c9eeac41b290a3712d88851518825ad78f45c71";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/object-enumerator/zipball/5c9eeac41b290a3712d88851518825ad78f45c71";
          sha256 = "11853z07w8h1a67wsjy3a6ir5x7khgx6iw5bmrkhjkiyvandqcn1";
        };
      };
    };
    "sebastian/object-reflector" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "sebastian-object-reflector-b4f479ebdbf63ac605d183ece17d8d7fe49c15c7";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/object-reflector/zipball/b4f479ebdbf63ac605d183ece17d8d7fe49c15c7";
          sha256 = "0g5m1fswy6wlf300x1vcipjdljmd3vh05hjqhqfc91byrjbk4rsg";
        };
      };
    };
    "sebastian/recursion-context" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "sebastian-recursion-context-cd9d8cf3c5804de4341c283ed787f099f5506172";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/recursion-context/zipball/cd9d8cf3c5804de4341c283ed787f099f5506172";
          sha256 = "1k0ki1krwq6329vsbw3515wsyg8a7n2l83lk19pdc12i2lg9nhpy";
        };
      };
    };
    "sebastian/resource-operations" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "sebastian-resource-operations-0f4443cb3a1d92ce809899753bc0d5d5a8dd19a8";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/resource-operations/zipball/0f4443cb3a1d92ce809899753bc0d5d5a8dd19a8";
          sha256 = "0p5s8rp7mrhw20yz5wx1i4k8ywf0h0ximcqan39n9qnma1dlnbyr";
        };
      };
    };
    "sebastian/type" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "sebastian-type-b8cd8a1c753c90bc1a0f5372170e3e489136f914";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/type/zipball/b8cd8a1c753c90bc1a0f5372170e3e489136f914";
          sha256 = "05d36w28nr2i14nghzd279gvwwpcavcznb2h5bp2iy3rhaa14yjy";
        };
      };
    };
    "sebastian/version" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "sebastian-version-c6c1022351a901512170118436c764e473f6de8c";
        src = fetchurl {
          url = "https://api.github.com/repos/sebastianbergmann/version/zipball/c6c1022351a901512170118436c764e473f6de8c";
          sha256 = "1bs7bwa9m0fin1zdk7vqy5lxzlfa9la90lkl27sn0wr00m745ig1";
        };
      };
    };
    "theseer/tokenizer" = {
      targetDir = "";
      src = composerEnv.buildZipPackage {
        name = "theseer-tokenizer-75a63c33a8577608444246075ea0af0d052e452a";
        src = fetchurl {
          url = "https://api.github.com/repos/theseer/tokenizer/zipball/75a63c33a8577608444246075ea0af0d052e452a";
          sha256 = "1cj1lb99hccsnwkq0i01mlcldmy1kxwcksfvgq6vfx8mgz3iicij";
        };
      };
    };
  };
in
composerEnv.buildPackage {
  inherit packages devPackages noDev;
  name = "pixelfed-pixelfed";
  src = composerEnv.buildZipPackage {
    name = "pixelfed-pixelfed-eb84b6547d57db1d99b766c2efb012bded208dfb";
    src = fetchurl {
      url = "https://api.github.com/repos/pixelfed/pixelfed/zipball/eb84b6547d57db1d99b766c2efb012bded208dfb";
      sha256 = "07hjdfd5ibqa5nnhpb6g7rpj78d82g91zcw70jbcb231pi60xgqf";
    };
  };
  executable = false;
  symlinkDependencies = false;
  meta = {
    license = "AGPL-3.0-only";
  };
  postInstall = ''
    cp ${pixelfed-env} $out/.env
    cp ${pixelfed-app} $out/bootstrap/app.php
  '';
}
