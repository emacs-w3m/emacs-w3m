;;; sb-yoshirin.el --- shimbun backend for Yoshinori Kobayashi Official Site -*- lexical-binding: nil -*-

;; Copyright (C) 2015, 2016, 2021, 2022, 2025 Katsumi Yamaoka

;; Author: Katsumi Yamaoka <yamaoka@jpl.org>
;; Keywords: news

;; This file is a part of shimbun.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'shimbun)

(luna-define-class shimbun-yoshirin (shimbun-japanese-newspaper shimbun) ())

(defvar shimbun-yoshirin-top-level-domain "yoshinori-kobayashi.com")

(defvar shimbun-yoshirin-url
  (concat "https://" shimbun-yoshirin-top-level-domain "/"))

(defvar shimbun-yoshirin-server-name "小林よしのり全宇宙")

(defvar shimbun-yoshirin-group-table
  '(("all" "すべての著者"
     "https://yoshinori-kobayashi.com/blog?author=all")
    ("kobayashi" "小林よしのり"
     "https://yoshinori-kobayashi.com/blog?author=kobayashi")
    ("takamori" "高森明勅"
     "https://yoshinori-kobayashi.com/blog?author=takamori")
    ("sasa" "笹幸恵"
     "https://yoshinori-kobayashi.com/blog?author=sasa")
    ("mokuren" "泉美木蘭"
     "https://yoshinori-kobayashi.com/blog?author=mokuren")
    ("kuramochi" "倉持麟太郎"
     "https://yoshinori-kobayashi.com/blog?author=kuramochi")
    ("oosuga" "大須賀淳"
     "https://yoshinori-kobayashi.com/blog?author=oosuga")
    ("chinone" "茅根豪"
     "https://yoshinori-kobayashi.com/blog?author=chinone")
    ("cheb" "ちぇぶ"
     "https://yoshinori-kobayashi.com/blog?author=cheb")
    ("mah" "まー"
     "https://yoshinori-kobayashi.com/blog?author=mah")
    ("kishibata" "岸端みな"
     "https://yoshinori-kobayashi.com/blog?author=kishibata")
    ("gosenjournalist" "ゴー宣ジャーナリスト"
     "https://yoshinori-kobayashi.com/blog?author=gosenjournalist")
    ("kiridoshi" "切通理作"
     "https://yoshinori-kobayashi.com/blog?author=kiridoshi")
    ("tokiura" "時浦兼(トッキー)"
     "https://yoshinori-kobayashi.com/blog?author=tokiura")
    ;; for the backward compatibility (reading archived articles)
    ("blog") ("topics")))

(defvar shimbun-yoshirin-index-range 20
  "*The number of indices that should be checked to detect new articles.
`all' or nil is for all indices, `last' is for the last index, and an
integer N is for the last N pages of indices.  This overrides any other
index-range setting if it is non-nil.

If you want to increase this value or change the value to nil or `all',
you will need to re-create the group(s) (delete the group(s) completely
in advance).  If you don't do so, you will never get old articles.")

(defvar shimbun-yoshirin-x-face-alist
  '(("default" . "X-Face: SX1=HV&[cI##/uM,,hed]\"a1.(e<M21Jl\
2dAy1-JnWwb'yT9t|fq*~ZpiUV%bx\\@&sdy`Vb\n c5[lT$}jl2|X~V97O'\
I31<&o-eCYZDs~WZVK{m,T}x>b3T9PCilX3;\"*8oF;QS\"GCHWit%'u!of`\
\\p\n &ute*s]IuWa5co-wMr4X1dQqqx/PB3y,@P3~Cdc<:$9.Jp^X$-*DPC")
    ("\\`kobayashi\\'" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAADAAAAAwCAMAAABg3Am1AAAAtFBMVEX////8uAT8vAH8vQD
 8wAT8wgD9uAD9uQC1k45cPydcQjj+twD+uQH+uwD+vQD+vgD+vwD+wAD+wh7+wwD+xABcVFU5Gyj
 /uAD/uQCCPlL/uwD/vwH/wQDJmGb/wgAoDSPuxGWDWFfpwq4YDR/v07/wuxvfniDv6+vNopSFcVQ
 sIC3x2cirflSIaClANjJTNUmIfYLRvKx5SDD15q7kxZX269QgHS34vAOxgSdGHSlZHzXVyMrkNCq
 HAAAAAXRSTlMAQObYZgAABDBJREFUSMeNlot6ojoUhaVgG1GGoo1VO6jhZogQIIACff/3OjsBrDN
 1Zs7yq1pZf/aFbeJk8l11Oaie/Ft1eVy+3bQ8lv+wH42337Ss6r/Y31ZK+9XqbXgLMqo/+Mshl/1
 qb4RhaBjT6RRgGaV8vHxv3++nhhsTnEbhdAr/yQ8fBOn9+/1q6hiGm2fswngaXVcyOUkdv/n3vVZ
 h1CRxzhjjnGcpEG/9heMj/0/HiJIcZxmGAIxlmDcGVPGA6P0O+AnGPONYBQDCDK/7n4PuKi8/1Cf
 ONMKMy+wxifOYmACmzdVxeuarV7Wh7I4TEvBzjnNEq4omicA+40BMHUXckjoOMR0IwHCMCi/wvIq
 6CTF9qCV6HYAxqXKwO06DuYm8IDiPAPcvF9YYzgAs+yk5Or1eDQAIDc6gwKM3wJREr7Kv4LWXHpo
 Zj6kniUARsWCSEKE9WJYqI3vQNYFWJhVkdJZZUYqIMJnvM3HVX3uLIXM62vbHh8SvKdwrVCm/5xU
 xJuZMcN+/zELd1nUbwtiyUcsPAECvTw30lFYFQoh60NQ4FbOZDMEj3f5hy3VtyKkeM7L1CHMMibj
 IVYAwZyY3+aVtQv3H4DGgBP2ma5xh13WTVAhhEhgq08QmZ+lVt0YLFFHp1qgQM1NQCr1F21wkCSE
 Cyubp9ebQP8pJBa8/lKxrw2LTTWQNRUGgCGAkoIFDmSzdVsCuB3ZWaMIYIQqPs5cIQmIJZDLCsKS
 uS2B3A7QmphRWL+jZcwmC9wlUIyM8gWu3g2QA2N1kvWjQH1ABgyRE4kGviBCp9mQtFrvNbrPZLMp
 J+QUsrKdQTYbnIZTHCVV+ABZWb9jsfgE2mxfr6eipO61mCcZPEs0AQIidVk/qxeZOFgCBmqaCoiR
 xk1iIaGG9jNfl9C3n8/dR8+dlBTUEQYHIVpgxjSXw/HIzyFk6Pt/4zctcq5SdekUSq/sgxPX5/X0
 0yC9EeQds5nOZUwUVQENjl4gZAJDCmJH6yi3vgM1c5gQxYPhgrAQAs0abz4er/TZQzb/0PuQkS0Y
 ohvEWfN0TcFXrd4FauyOew6quJUCTGGZva5prv4205/kCLo77zH0IrTrX9TlQXY0TCIGZf1inkQa
 MdtvJljd7FAWFd67VfYA2zVrp933/MGsi7Wt3LVVSWhil7QGmFWLUZw9uRdti7PdA23Xk7uyqNA1
 2+e22a9dZntBaETThOZZuCXRdV9xv3xTcWwnIjSMOFFEg2O/7ADIC+uWAOKEekKcC2iKZVJFdLtJ
 6OKgn9NsRpIhszQ6Mwyvyzjm7AADOdduu1x39fioWqgRQt93iAkv/4aAAqPfz0bH7mbdtB4+22+Y
 Y7Bc2Avnp8UF9KnDXZRn8qfUZxJPA4+VHJJelZFytv1Zd/jz9/efGucgJyRiDYjKSF6f/8YNmcjp
 9Kp0euf8D68DXEs6Uw/EAAAAASUVORK5CYII=")
    ("\\`takamori\\'" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAADAAAAAwCAMAAABg3Am1AAAAwFBMVEX////HdkxGike1dkm
 0qGpFxlUzvVZqx1K0v5lJWD5Gu18itVdLLBfJiFAnIRFZy2VHxlXtwpHurXXt07iVVjsktlo3uVZ
 9fW03v1Y4wFZPQCVPQEDgmmleyHs6s2BMxmRx2lA7wFY7wlZ3OSt2Wj08ulY8wFX0u4Q8w19BMjG
 IjETjqnM9wlUwKyosuVbBi2Nj0ndBf17msoOyWjlBuFKfhmPWkGRUvFN5tnpUzVNCw1VCxFV52np
 DxFQxuVahjYGI9Xo3AAAAAXRSTlMAQObYZgAABFRJREFUSMeFlot6okgQRhXESBBvEBsBr3TwAoq
 ijZpofP+32r+6MXE3zk59k8zkyznVVdVYTKXyKy7b9ngx8AaL8XB7qfwlGtvrRxrPPK9er3/ha7N
 ZXP4fJz6W+EbF29uflW3n+vmRzqaeTE3sG771z5NJ42n6tmF8plSPSg/cst6s+stkMjk/OeTSMa7
 gU9Tz1e/3vzbEW1Yd+cnYPuVRP+rpHzTGTid2HI2sTedloozzUz6det5BMzkTNwRj2vF8vhvtZ/n
 jA3Cfc84QQgjGjsfzkzMaqv44fmWa32z6Mvi3odqYvPx0fpb8LD4wxpuIVktZOOnGIJTG93Qvih+
 gWa6yN/0IoY45Hjudu1EKY8V76Jb7zRYSu3t9r+suj/DvFYS7oYraGld5v33TDwIkd7MilFHYe5x
 iPhpSoIKQv36Q/UauHfZ68x4iCW098gNOQmlQFxfKD76urVvo1q0WSW+O6M5hFG7TV4IyqKZ2yX9
 p4FtRtQgVTzEP7ajlax2jYxjSIMFIU8l/0QlN1+6BJWOeJ0nSC/VmkwR1xgVCI51O1fOJO2s19SL
 vdnOc0UMDWZgnRdTSJN4Zt9skXGQ99Dhr/pqEJNR1GyNC4Ig88UthDGELY3jnIbRICLOMYAy1CBO
 IkXks+e2WhPGdl0IEKsxD2w7zoorLK5Jc50ejzC+FxZ23rCWEDJmr+321yHGKjYbmGQl3fjuulB9
 H+nSZVFJS2LpetcNunvR6NNpw9fLAz6TwpoSRH/kZ9YnHInccajmfz5Ps5YFPK4tvfvPBXV9P8h7
 4pOs4uLUQT0menxS/BS8FhVtf187KjXRMJsmLcJ7guaji2sNe91TyRkrCuOQ3GMXRdfeYZR5W0Td
 1blcx3+Sk+Ffws1llqPg63b6xwp3ZwKohTcq2bfqxJvm2xCFc7rxhGNdjhhHp+71Nw6Ug4ST5D8D
 TGEKFTqhTegOLdVUlfr93EVHkYr6FFNrpFEIcx1g2I8vySv4zPWioCuW4EdqRRRUZjSdNJT+d4vE
 eWp4hcbUpjxDwp1o2YdsCfBzPplJY0Ec0luXLxYdV877CBtDp8dAxIrTNjrguKQwGg4va8pL/lMc
 OBgfaGYUK8JmzO1AtJHgLtWY6Pzy9ezTqNiOaRpY5zNRe3yEMPG9YLjKM54P4wVQuA5oP1SUHpjs
 sMHe7V3Km91VJLxIpIAnWE4vIUBEJhwcB9vlutxudvpcx5UcCwkfmUkQRFzfB5F00HDNQwdnDusc
 YMAPgmrleCzfi3DQDTgv2RwgeXxBD3Pv7SNuZy/W6xW7cXyNoI/NVw1yqA/79Zhy+j3Y75CIhECJ
 YL5fLoInSeC1Qwn/fpCeTjl5CWC8F92kLtnxRc11B6m++UrlJXhqCB1IQeaYLToexpy9qyZPCBMd
 fTd7Ia5lAO895OsRUQiBqHJs/yPNaLv6MPyjCYXjNLRvdvCvY3/5Dw8xgyZwbWl42HOfXr/8BjB3
 hJNqf0lkAAAAASUVORK5CYII=")
    ("\\`sasa\\'" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAADAAAAAwCAMAAABg3Am1AAAAvVBMVEX////YqP22X/5bTUa
 1l4KTVkbJgWjIqbemVOCTcWjdm3+VakqUkJC6af7Ypf3LnexyWWnNkv2rTfzOkv6rYkt1UUjgvv3
 Plf7PmP6+cf6+cvytTfytTv2aZqNSO0bhvf7QmP2uTv6uU/7Rnf2vTvyvT/6vUfx3c2yugWfSm/z
 SnP6vaGP2xqiwVfzToP0yJCTUoP7Uov3ms49FMC6yWP7VpP2fh6XWof3Wpf6OdoHWpv2zeOEjGRv
 XqP3Ggf6M+fq4AAAAAXRSTlMAQObYZgAABBhJREFUSMeFlomWojAQRVUEgShGmzSOLKPdCihCiwo
 KCP//WVOV4NLLzLxz5Ci8m1oSEjudb/rdrffbxWz2fun+7vxPr90s2W43C/DPZpY126bd13/akyR
 5FwAgi80WVb/+1Z7u94kIsFhskmS/B/sGqJ+jvGZpgnp/32yTJAU02XIA8R+K6YIfifScng0jS8/
 ndnywW9ai/uqvs6wsy0t5yYzxuKrEB0bnfiCs/ddWouq6zIzKDQIKYrTIx+hHu2W9pJ/yEaq7xji
 nRSBJ3vEoSRKjFTaMA5PJ+/fxDSN1PQ/dKA8QWt0CvLxMup8DoL/Mjx9g/eAXjIHEHXh5/ewvjbL
 yPriuiAiimPCEEGia54S6UPCYCve1JURS1gQrQH9T3wPU2KBufrzCsICAbiFoMZ5YkE7DJVYEqiy
 zsqYf1yMjxxsAgkZp+WRy8/MQbQEAjL3rVZIJOyJwFY1iTCvGmM68mYOaFgB7lhk5pM4Ikb2jiAA
 dZhILtIp7hdqSLwYAmQsZyYQQhgRUDYAWAOA+/L+sDs5vfQF7mgLgoV9mkJbneZB/2NdoEFTgbDX
 vYD7oh8UKAFgZk4kcEpkx0tcPB51R5t79AEC9F1iiAvggKNOUwxDJ8LDb7TRKiwfwq9OF8c9ZegP
 QiBH0iEmajkBfC2g1ukkAWECanGEhkZCxI7RW0RVCFPTv+iFlD2DUuYC4P0kr6ahB8rJsmsrhoIQ
 Kj3CIGH0GDOOcZWf++m8oABEBv6nvdrrSAgqj6nOEDN/iPewQkJMEANjNiAM6B3a65PYeQoD78ZU
 fU00RAKSPEXa8CEl9BozLHVg0LuuHCJihrt9T0ugn4AwFA7GYNPPeSaV9npNs6ly8rcztne7qYdE
 bMI/ETfeWU4iTDB+YOOaengFrPjrFcfszjt1Q5BRByaGCAJXU+KGmM3sDxW+tYtWNTD4Vka5EfOY
 k1/fhCb+8vcH7cFqv4Ze/5vJ9FWYCVhNOnY4RBq7qOM4SHA487gHQLJd4B6/Lle34bkhESqKGago
 3Hdux4VF84nvAemUvUQjBN9/FsnFdcA1UG922DfbTiO8aI3u1clZLoRU8caMoEgDGyHuOM/V93pS
 JOBh824YgGAAirOwVxkCAT10/H8TruO1ju/PNYhjX5vkABqgNlUfYX0XvD4f5oOq1c3BTE8c+5Ln
 ijL1c22ofWyubSlgMhxzBGM1j+z7BRODYkJo9napqEfJXNTK1oVCeV2rv+YDo8c4601hV83xYSGK
 6iVYMb8Rg8PkIepui0I3yxAKRiyfg6yHXA78btAZKwig0SVDcifz7MTqfqkXQPg5ICBtN8AAGPx3
 U4/w+XqEp0CD0izt/++swEG5aFEQJgwAj/MvOBf4AzlFNK27Af//QYJiiEPn/kPofM+nhfjtEAPo
 AAAAASUVORK5CYII=")
    ("\\`mokuren\\'" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAADAAAAAwCAMAAABg3Am1AAAAt1BMVEX///9tO02jc18mGRb
 92cy2j4LudKPvYKK3g2/szLrdXqPwYZbs6/DwbqLt0dft2dXxV6xNLirxXKE7JCGneGvxaJzxbqL
 7z77yUqvyU6nxdajyVKryVKvyVavyWavekJmWZ1ju7PmXUE/ybp/ydLPzVazv4djzY5fzZ6H0X7A
 aERDguLZkPTEuGxZ2VVLRdYP1krcvHiHjsNHkr53SqZqvdmP19fuwd2vUlIHoY5UhFRK0WX3Xk7Y
 z//qTAAAAAXRSTlMAQObYZgAAA9RJREFUSMeFlol2okAQRdkDbpERJALNIEtMZBcQhPn/75qqxgV
 MJvM0yTmxLq+qurpbhnmWqc/vOpg586NWpn6YP8n/IT7XQQfQfIKZ/wpf6jc9u+Tfx/sPAl3mI59
 vTIItyPf9K3EBZf3lUf1zvOO8LLcDo+uZx6obdVMUG7YfXA5z6en5FrGcj8Hj4qlJ1/F8lxSA3F0
 mRBAS27bXL0hcWDXhj8euKPii6PiiP3wl8jAkhNghcZbbS6puNuDQFRvICQj1civ+XscqHGQTe/2
 B6QMB6eAfvug6dlga0Ome0BAPBOnjWB20gbKTJOE7Pruu5m3RT1cDKIPMWARiFBoVPA/1eLcBOOR
 jA7CwqQG8UnjFA8An3eZyW0x9bAAO1tUg9VKXpRbY36Trh+U86Ho+MbDQAJ4fe67LedQC28V37FL
 3PyiDjQofGe1SdIhTl9M4DatIEACPzN9+DMQoIwBmLAtE6mmN6IpYBfSIWng+zI0/APkDID0CaZo
 i0GhQDRoc4SeWhqnxIafgAVgeGrBu6mqiyLkpFpQgcVQvdDCXvj8GLMuLwUAsW7mKSrGuG9d10w3
 fHY+ptBxmeWkynyOghw5pUSS6rWJUdaOl0C8geL6XzBvCOJOi1TgyRDF1I6Gq4fFpCr+KjSqZIEl
 C5NHVgXAVQ4naShEqGcSBXFh10xyIrTQBYFhnrQFSDAGAtqpaUW64NGbNmyRpVAPOEvltCIgIRiW
 LVVm1MlTiZdKdGBdNkUgQgCjhLddoIdeaq43ix23F+LccgbJUlLaWZQCaWuP63UtgmsEVyKcOFIB
 lKGXPrTGjuuEyGw6IYADyZ8A5oQO824arZcioafodHBA7Z0uRnBm3CQjMyVAEI4JYjkMgW7/ZFmz
 3HfkEAra1MwHIktZQtpnneS4CPYyABQTZWcQJGOY5J6wCHX7LXM3Jjez2+E+b0C1vB/SUnBIkh2W
 AwmH6QHI8w8OEUPcwPE0OgWtSVg6AAFUjoKUA3D90rgfxGIBcsbWCQgGO6y3yAE4M88XitngRTB/
 ET4DgdlaOnw8tpOOhlLSxXPMLSp4awEFwTQpaQd4ILQGXToYSuDpbY5PQJp8c3xgdkt1uOcQPS1d
 roOzXG7Y1/JxcENA5axcs80gxroBSyU2jUWWztWMFT1dQ+BmcFu+KYowAeQBg64nVn+dL7rTa7/f
 nhaLc4qNKRAuMFsXq9PUaPe+ROL+jB27TCDcojRbFaPXdRX0lzu+vCqiscEtTVafFv7477M9InM8
 LgKIKFUXR6v2Hbxvnh/ZAvb6uXhfM/4TBqAXV86d/AVEs28Gik0ESAAAAAElFTkSuQmCC")
    ("\\`kuramochi\\'" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAADAAAAAwCAMAAABg3Am1AAAAvVBMVEX///8lEgyTIjrrpKu
 UFTOUFjJwDScmJjEmKT8VDAq6HkKUTjSWFDNyDSe4gG+XFTPuu6bOJEqWSlW8JUZbHipgPSsqIh8
 YGB+5qqw9HSXQKk6Xd2o9OTTf0crgsat3DijAIEQJCAu9lofilobAVE53YWvVJ03VKk/DIUbkq5L
 WJ03WJ07CX3P2ybHTmoayJELXKE7XKlDFKEvVgHfTx7WPFDF9ECt8QiwQCgt+ITOjIj7n4+dtGR1
 tGTJHTFq4/R9LAAAAAXRSTlMAQObYZgAABCVJREFUSMd9lolyokAURV1RJCIR2WzABISRRMDICG7
 h/z9r7usGNZnMvGhVqryH+5Ze6HS+RbFdPt9iuS06/43+oxqx2Wz22/6/5esl9Pveaa/r+Gw4QIj
 /s34bx9DnPcc9fZ4+HWeR7znwMivZT4//iGMksT+5pjkyLVmWu86Q9JPDqpSqv/W/YgL0s+FCbhF
 wlGXpZXMuV6tyJX3aX/VhzPV5VbsjIkx4HEFch6uSQpKk4Mvzd+9E6JXhmtYIYZpwAHF0pi0gPXj
 4H7t3EPvKxdNl2XRd1+zKIisH8sNhCmB6Bz52HNDp+TIB+DM5AhMJ+gNZTE+34e4AvMVLw3UdEpL
 ccQVBFg0gTVlbwI475AAoGcdtSMfsgukScCD9dNUMbMeJuMf1rhExFkVKZRiO06XgwJQD85sBUlo
 aNeQVUykYmytKdYUHdxgMyrvFFmIOYAaOqxZZwZE5JwQwGAyanJho0a4F3CrIMkEwELAgAPqnBig
 xY25AXTrXhhFlSZKEBUJV4UAWBAxnT3wSq1VbMgF5XUeMAE4gKQIWXecweJrNZqUAWJMRtVWvIwG
 EGQcYdQqt4voGwPDuwBJ6NdE0DQ5qERbMMDgwJP1MFIE+3YF4DgMtCQIUXhShWilsXlWLxYEDA6l
 p7MethjiPmKZlNDZWhCG1ClUYDTBbPQLvtJQmuQpA8xN6foIiAsO4Vsa1AcpH4O0tfs7PecL8NEV
 Sihpq6nytsjU1qgEOIie+tKHfnPP8HKLmpAigBaCoIqkWGEqcAAA90qFYbwEkIcaQaGo1V9HcOzC
 btg7v789cPpnoYZJSU1ECAGWN3irVqdHPytYh3nA1RaGlaZpwAFIBtAaiCKnzwdMR+sm6D0ALi1B
 Lk7WiqJTS8BsAfaOevEz0dQIiLFAIAYqKNrV6MbpTZ3uT89gSkPh9zAGLb75WbiXMnmh0rFM8qBE
 6yoYBgO26qrD+nOvwsWpsIP1BThYaUlITzHt+UphiHOV2bVARn7RFScZPaYpJL8By7WPfqfMKW8j
 A+dddlORCu462qP8A5D2jpvWkaX3s0Yo2xJFCdq6HIZ014pjZiGtAx9UwHo/tVMP0Mn5uYO3JRxF
 dOnMicbLi3tDPvZM7Ho0J8GAQUkpYeYoAXgVktUfl+dMh7UgAnRRV0BbF9onc1oHidhG5ljUa0yk
 vgAst8owOs6pyBACL19f6dnrbI34pCIB1Lp7mZ0kWkIfRFQDCerhQ7NEdiC4XH0CWBZzgFqQ/frm
 yGg8BeB7pkyQjE8Xoyn/rbx4AauhTDLohVDTKsl4t+/s1+tttHbzU87AEfR/Nyug8W1hW/cNFfYk
 EUHtplgnGS33eXZP9/Cpg1+Mmpcy3icBXQ3fZv19O7IiAS+dyiSIMgzySwPv/C40deReEzSIW2Db
 +//b7H2lV2h+mjgEvAAAAAElFTkSuQmCC")
    ("\\`oosuga\\'" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAADAAAAAwCAMAAABg3Am1AAAAvVBMVEX///99eoYiY+giZ+4
 iae1Gi/BaZYYjZO0jZu0jaOkjceiAbGmkhX1Hp/IlaO9vami4gWWlnJc6NTkma/E4eO9fTEhZX3Q
 ncu9fXF8nfO4patdPT14qa++Gd3Yqeu4qhO5Pfs0rge8bV+h2fr4tbujRqph3cXIcXOhAhO8dWuy
 cnM53rNIeXu0eYO0eYunTwr2fZk1Ci+iffWQfZOwfaOigaWIgYeggY+pXb6J9VEPFjoMhW+h9X1h
 8gJwhZuzbWKH9AAAAAXRSTlMAQObYZgAABEJJREFUSMeFlol2okAURAEVF0iAKIImjRAW44YERRH
 R//+sqdeNxklmqePReHKru97rBSXphz6CmbdLd+12exh1pP/pI4C8w4b49vz9/f38L0/H84h3Npv
 dtd1+Jv59Pj9//I13PI8czjp95OfpQfnjLCPOe56zhqH/HJ0FfkihQ/STfxs6EPiFEo38MAzLspy
 e5+lxA34fzL7zQ847znAdhasl+Mtl/DpWp8oxnaeKN5vNvvPC0Dr74ZLoCxmgcprO98T/7ngbCkd
 r7a/ChheGV0wi+EfHx5Ab3rrzOaXnlrAxvKq81WR4uvGroVB3sTivlstleDOp5Bn7jeHpafUYaPi
 8WSwWyvKuEC/K5YYBdzxBzfqK8ddou+I3+aElN1zkcXnnG8edV/zywg1lqZLG47FKX4PJjZ/dJ3h
 bL96nwG8dEgYXSwGT7934Waep4HmN/IhzK9Qm8R6NyVF+Et6ZIJpIxPk1DOrrywvQuj5Bto0vwlK
 C/wwmEE8k+LmPlry81Iwx3aoq08xhIGGHqOXT54SrI41QL/D1ehOVxOtMZ3Wtx5Ve82A2OcYq8Z4
 3mQTSnd8oZLDdOI71SqtrO6/yvLBrmuNVjSa09SnT85prs9vt+mWh6ZpesBNIvdZRSlHXPFUZic0
 Pw0YMv+tCkcxiZImRP47rAoXktQmXrU75XvYGXiAJHHwPClm8teCp2DY+5VVVWaZZ1Jk6HQwGzsB
 xBjhIPE332uu1Wq3eyNputzq03cZFjc/CrIosjAaNJtjjIg1Gv1673WsYw4Gy8Waa+MR7pkUOpx3
 iYaA0Vxr+CvlkqMwtN1TbGMuRydHX+DAgD6Vp8Tl2IwxaZWaVm0WWFRVcZu4/8jMJ8Tnf7XW7x6N
 iVWaWoXLtdMqKDLzJMEG/3x80B1V6bsHA81+Px+PBqoqTbsWWptF+wiy53+e6H+zR3QA+TUPMUOh
 YgSITBpnj+0kg+I60okAoF3mgnY/FqtAl9CfLYMjK9uP4Mxxr0SDOpyjikpMB7cm5ofD3+/7+ztO
 RG11Rb4PjbcpyrDTPRCr7+/1+wE81QgV0ka+uPV4uj3RMDyWzrAo7Ki+KPCumnJ8FjfhF88b7c1M
 6lS1aacuysDNypRm/kbjH7vBudzwoYU4bKiZDXuflNPrOowoRH7EinOvYzC0hVmemllx8hBfn8/Y
 oWiqcVnDPyLKMhagZ4ui02JnrGkwtfTprk+B+GSOUMvVLmTFZZjoaeqrp4kCTcHXYmqElMnlWX9e
 3z2mWMHozxSo3vGtrrqEZRnIJHx8QOqFGwhJIw5bmouFd1zZcAzyTf38EyUkCQ2Iw/C/Jc5OOsy0
 MKAO49f0hpxuJCwNeJE3TQN4MifyTlyQrwUjc4Loublf65pISV5a3f3ywM27gUxCHv2SqSTasv/5
 2IEsjkC6TKYy1/dePEyv54g10Trak/0vm3TUMDP7zn78AHZzo9Xa5IaYAAAAASUVORK5CYII=")
    ("\\`chinone\\'" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAADAAAAAwCAMAAABg3Am1AAAAvVBMVEX///8AFSYAFysAGDH
 YsqEAHTcAHT0AHjoAHzK1fHAAH0AAIzkAJUIAJ0oAJ04AKVMALFgALkwAL1QkSGgAL1sBGzIBIkQ
 BKEYBLVsBM1varZQCJkwmQFsCJ1ICL1ICNFomX5Ls7O8DNWrKopTt5uIADxsEMmK5lX/Kxsrw8vX
 f084IHzPhp5NAMzocM0+arclUTlBCRU9mcYBDQUB6XVjmsJbVkIczLzXVppAhKzo0ND3otqSyn5j
 XoYuPgILXBkp1AAAAAXRSTlMAQObYZgAABERJREFUSMeFlgt3ojoUhRGhithqVZSh+ECsUaQ0vJQ
 i5P//rLtPQG/b6cxsC3Xp/nJeWURF+S7+Yem63nsgRYnyD8WnrqX3yD1oNY//Zu/CTotL500D9w/
 2RO1aVmsn46OUZH7M7EPttvb5PHLj2I1eWgGKfvefLLk82V9izvHH42i5fILAzF+++187Vq8p0uX
 x2wWvNAXjrkajETGP829+SO3NsVbJ0wv8uAjgfD8eE4NvPvs7r1LqcOKm5H97uwPJ3h6DWT59Jk5
 dtUE6p7RJJgUjgTiZzmYzWyKre/sfenqLlGm7ME+BZZkMsd9L4ulp2gKYzaBnEdKRrkxkQmSZfIN
 2TbcggKCSxh/J6UgEFTPH10iFz5iv+VnKk+12K4OMRs04UA4hgwfd4jzztWurPMeNIafpnZAhoqW
 cjYzCMwZ/fiNwaVkiCCCiDTGiNqNrFIWAPM/zW4j8qolEbLZ3YqkoGzmYEbqGMIlgjhPmYd76c+1
 aCbHZtIA9Gk2VmU2DIWhpr4TwtboOwxBr5yH9DzRRTe8EhUDDbGJG9h5+pvmGYTCHQjh1XWtBIKp
 Td7hvsiJgL5ts7/f2qjREpTEDmI8IDsM4qiCoqqynW11zBWhvSwDabn89qm8GMnJqdi6QUu44Rc1
 YsBPM4JiS3u3qi5W9VygSrrWpWqVh+NrZMLhxRuG1kWVGfd0J3+BPL/P5A7aPag0Vmdz22bL65od
 haAUSZ+xch/WZGex8dnaMGS7tpEfaDKraAGa/bw6HEQAkEhZhjc6ea5K2Y0a23c7QIIRBZspms1n
 Dbg4XiwagdqKp11DqGjDjMsaatGGXmC2ABa2+eIYyCeQ0ONxQeehoADqvAyqTBrd8UrYmrQ73CnJ
 9ApARItR1C2Qd7PulJGbjsfLJvlo9V36BPOoivNYhtcrRtPhkqUAsm/Ka2QpyJ/tkMmkAqhQtguh
 doVUmxqYSMqfJ/VKkfbWeSOJZSKCoyc5klxLT7PUbQualrLH6miQJToBhYH1sKYqhxQPT1FuC8lK
 U9U0ERLQsjG1GdS2GQ3RF160WUb4AFIIIlC2v+qyJBQEg+vSU6BCw+UJEFUOPCgKw9/xAuBKQBGL
 Iw+ILMK18v2iFvRpwHkmg1xDNuXAHVpNVojl+K1YFnoh5PLwDFm8eTPsbgWMhCXynKOBm1cHzDmU
 cp3ELWNapffIJMrtuhG9L4Wm1rDbwPG/HyzLFa0g16P3+/4dVDJUX3C6V51WMaTuP/OJSSiErAPq
 n45E3n8clRx7vFexH+CucEvT5pUxcEF+OrZj8aYqMvPf3d+9I/kN8Ay5xkkTfjjmexmmJAMcAwOH
 YJNQAdOfJbwdphuouh+Px8E4hPOqQBC5S5Q8Hr8DZAF/gtUByD4AixI8nO0eLdt7xEMgArZ9q5n/
 87UD+o/R7vM0F3f7bj5MjRFATAEr/+XtGUQ4EJJcy/TGT/wABCvka9vrNRgAAAABJRU5ErkJggg=
 =")
    ("\\`cheb\\'" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAADAAAAAwCAMAAABg3Am1AAAAvVBMVEX////679IAAQD68eP
 pypj75dz8z7QBAQB/XTCkdCrr0X/r0Yb939f949n95dv95dz95ttcW12TZyT+5tztzLOVaCUAAAD
 u1oeWaiXv2I2onZOopayGWh6YaSWYbCZzcXnNup/x2o2GfXzgwG/y25Hy3dby4JSaayfPx8SbbSY
 LCwqIhpDRq13jxXn15ZudbSadbSj17ravjEeebyn32sqMi5/m18n46qGxpHTnzHtGPzjWunN9cFZ
 ILhKQYyEQoV/mAAAAAXRSTlMAQObYZgAABGhJREFUSMd9lg17mjwUht07+bAKEtSqAUmaikaobf2
 AWgT//896nxN07bpu57K7tu6+c05ODsFO50u8bxGPH9H5Z7xTfDG227/id3fv7zflcWvY7fmJ4lt
 8etcKj097l2J/Orc0xTdZpv1+H8pT3TiOc39/jz8zd//LOP/BIyDUGcG/wmn2fzEe5nMY2/1vuFH
 cb43l8uFhfnaz+2+iugmfjNVqtVyeUA0SZF+trMbm8TnE9Y1/foZxBpixrFaKfaoLO2fs6fG8D2P
 Eh/D8PG/w3w2XY9ses/9aOMsqxnKd13s3NjG5Cm+IukJB/DjwfM/uVlmTOVklWF5EOlesxePL5YO
 XjLGKpT6FZ0uszFhUcB1FFauaK365hEbYIeqqEiLy/dFoFPgDW7JKFJwxAQ87alr8s1BlWSO0H0A
 YDZFjrKqK4cwzatx909KIVvixMy2qU0pgcvhHeyw/upVd8XAyA//j5256wo5VdBNGwTAopZTVzXB
 iw08mIfXp524+PVeOI1P/JviWFqUuu/V1DO9JINxk2C3n8zOGjqephR4NR36Q8pwadrQlfl9rnsW
 Eo6DZJAGPweufqkxpGZWl5Y98zaOoNA0+2qcq1SVrsPoC+Gyx7hA/X8KogyCN8hJCYHGmLH+IdJb
 MNZeqmYFGgtks6cwplsvlWaWWVjrwaQtKCBKCoZ+WXJJANHLM1nhyjIDpk5YUVenhMHyYvucNKYn
 Fu2XdGHiWJOuk0yeDhNXJKpXSXurjHCxvMMBcDf1RxGVZNQmE9TpJZhsjPEB4fj5rS0dpoGnf/mB
 wJMMfcV1KJ0Y9wJPFet3p99sMGHFOSCohBCQcW8EqKwjrNSqiHJ27PmVYQXg7pQPPQkPNOKGkATo
 bKFU5Da1u8GRDwnRuErSCjHB8ELyBhwTDIz0n8RXfbDavRngg4c0ImArLKi3zYJAnwTemGNCbZHP
 A/dinDCsjYE0rirjmKZ0fTQdznNjsFzR+Xj4LO6mBpJHWEc9LTSc34NyJr8UQ/vJqrmCzawi73PJ
 LTIIuU8vSGD9Pym6zueIvwA+HVpheBcl9rVMqJdDorie5bdcbE4QjWoEaa4TdgWM26bHABw07jQf
 2aXHDD20CI0xb4bTv1agdw4QcmDrb9o/dGPyrWf2lFa7bRpe6+16v56oSE+TpnONOQ9fsusVfgR9
 uL547c3Rvp15vH15Cl3PFsLptD+jkZEirm+h1PgzU1MX6PbofLpfE7RoexXnj+nCL2906J+FhhQS
 9Cd0ns0VykfaxfcD9o3Qvi98SmNcbRpYS4IYLJzipUAboLhJY3hj3OXPdySe+Lep8MBWF4SLZhG5
 JkzG0vCMXoohyIUT95Y07pxah/HC2iV2mxkd6IsbAQSuhlPr6jt6+tAlit2K5wJI1bgIlokJF4EX
 +x2v3ZLbgMlEUgqlcFPSXQuWREN/yZBxclucFQglmymCCQuWq+P6rQ0ubQM1XmP719y8n0S8jpyj
 MEv/+PtO58oXB8z/x/wEXtORee69xYgAAAABJRU5ErkJggg==")
    ("\\`mah\\'" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAADAAAAAwCAMAAABg3Am1AAAAulBMVEX///8AAQD68fHYpYD
 rkqHrlaLrlqI3JCD78/X79/fsj6Dsl6TsmKTsmaTsm6TsnKfsnaakcU3soqXZv7Ptl6Parovtmqf
 upq4AAADvva7vw8Lw3dkIBAPiY4ercHPx6uvPmm7jboy+elLy5uXiq4PjpJbjtI7ld5LAmXrlkqD
 kt5vUemvUgZH28fDnhprly757WkPnq6/nrKHm3t347OzWo3Hpk6DpmKPqkZ/Xpo3qlKDqlaPqmKP
 qmaalFgoPAAAAAXRSTlMAQObYZgAABF1JREFUSMeNlg1bqkwQhnUBwZIADQ1BWEGRSJRCzA37/3/
 rfWbBsk6d805hXTD3zjMfu9jr/Wm7zU7aYrHp/dPUDWy3uxCLxfKv7u+bzq6IxeJX95ll3ZD3NoS
 VJT464pcopmVb1mmzCYN+v/8gf/Vw2SI/JPN8Nu9s27a2ZfDwYf2g/I2YjUbm1LLtRx1LfwIfxPK
 bLOt+ZAKw3ss+6XloL7KgVbX8RgzuEQGStkE/0PWALnKmRPTtsgWuCW1wb54R4VXvB05MVgLRy7K
 EwvBm2QKfeRwBQJJlh0FQ8rpWVitFUTiveezofb1RNxL4CPEugZFpW2Wgxxzuee55Xp6uQOBWo6q
 b5TWhAhicUVYACLBapfDfe54EnCB4VNWbzRUwAqANEMGSwKoF2gggdABa01wRqoyAHO6sMCgpQAf
 kaQcc3wYdQf7TDoCkaajHyBj++/l87nluxintx+OxaT6JG/UoAXR6Gpacc6cMhQdgEpZ6iSIDaOB
 wO91dgI8I9haA42RZ5nq0voL10RIkrZ1h0w31wgKgaoMGgGXXTowQ6AFU5YqSkb/jbE9v2uAWZUS
 MTe8Gkgg4o3HW2gmd2AlLh2duCG3yxzmeVFmVO2u6k4Da5WDa4yyTMWKlzEoEoj7EGXLWzoNblNG
 e7mSErqy2PRZpzN00z4XjhSKHKeBP1Kdbeo79cgHaaR0Ld53BP0fWbibEWgiFP3bA3R3tMCkJEpE
 Sjbeg5alnuZfWKf2z5msAg1cUEc8t6wK0o3EB0hUAhaNUL4K/mIhwHknAHn/mgKrZ9gsBeZrzes0
 dLvJ1vlY8b9AOJ3bw+AtgIQkXg4T1XYGp4LWiuO7a82Zv2hegzUGeGuM1CKWGnyueYgyWq7yg6aZ
 2fjWlpnHvdDodj29yvCEJmtxVje6iw2UZU9NpcGfIYUSHCoDZ6YQI7X6gG+sV9uZTrAgFLYSm5w5
 AFacS6F0AswXGWFQRT6h/TD0nQQAuZbdpWk9yekftyTeeZdDhCi4t414LaM1r2zg6wzpAnnyzuU/
 j6tJUCEwrFx0w6PpAW66ThLI9z/0kqYlAXck/5qE/pyq1W9i2Z90e1TRtZM6e937FWE4zmtFhhqz
 jLEn8ORQRMG0D0CnwpjXvc7+qqoQZvgDB5fGHP9nhkBTzprmVRZx1J/HxHWKwepUkB4PlrlvLlGt
 UasIOFUt8f25RhI93g1/tq6Q6JEmUsKHvpi72Tk2nZZwAOCB05T9b08/DGDf2SYIARRQNjb3IU9f
 FeCh8UrCoBeAxvzq+sX4VATCKZDhkExdECmn5gYAiwWJ4+OUFkRQVRRgaUTE02ERQJ9JJcmDQySK
 DVdXh2zsrkotg+QhXsRdA9vAvWqCI/nwrFhAFYJgYQ8awMHpI/hF0RqxgP7x2oxaImDEsDIOhAYw
 ViBBB5i9vdgkUkWEYxYHMAICcjb98dyCgogAGYwYJiqLkX19PhhUzCiiKZC2j3v+wYsgoBIgf3P8
 DiEjC49/SmA4AAAAASUVORK5CYII=")
    ("\\`kishibata\\'" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAADAAAAAwCAMAAABg3Am1AAAAulBMVEW15h0AAAH71B/8thT
 72yH73CP8vhb8wRr8xRmzr6f8yBz8zx3p6er9xRz9zB39zh790R4CAAH91B/91h/92B792yD93CA
 mJib8+/z8/P0DAwP+3SL9/Pv9/f0AAAD+/v6lpaXJyMf////w0yje3t6pnmLytRXNxXFiYVfzxxz
 zzCPy8vKZmZl2dnULDAn1vxv2zh72zz4hHhH39/b6vxRYTx1GRkbV1NX5+fr7xRuzlST7yRv7zh3
 7zyID9tToAAAAAXRSTlMAQObYZgAABJhJREFUSMeNlot2okgQhkXl0gRbokFYHJBW4jDQkAUREPD
 9X2v/JprgTGZmi3M8eqyPuv1dMJl8YfnbdaEvrnk++R+mKmU5o3qnddpFXH+Gcl1Vldms07uuE8x
 loSnKH5BFp6mKougCoFRAl1JRZr9D8gvVNKVUkE98bN/tbVbOyoUef+XfI4MLAlxpHJ+fn58lyXV
 duz0ql+5Szn4NsqS9SKi8HttzIUlZJvmZbVf2ue10ZPUL0dMl1TVVjV+LrMiyrLJdKStSTnjwXVG
 Q10/EEv4AtPg1sqsIVuEz5QEuRo4oDDHG/ubTckk7Tfu3ZZwZIQs5rxPnH+/EWVRF7eA/JnLTNJf
 LrjPbioUETjwKHW+323khZ7Z9fkOvQagfwEoAlJryueIpcrHt6LTb7ff7nROEaVUcBVCq+r2M1Tu
 wWsUZ2pLZWWEHzuCPEKROs6OiKqqqLy63hFYDsX6RW9fOXDfzs6j2PO+UIIwTGEEkIqjo4Sy+BYB
 N15Yln11X8iXXl1KjCXnKubPbMc4j1KDpugZVfgIv1mZ+OPu+D0LKSEKC2jglABIjIW+aRiGrcrY
 QVWxf1tPVersBUPiDSTYLDceoQ8NDEQHnb5Cijt4u1CsAy7JeLGuz3czjG1BFJ+9E0pSHzs6rOQ9
 i+GN48O8EMLcs3H+zvUeQOEfFPEpDhlkkQUTiJcU5uWjXbplP8s32ZpsBkHyXBF5DCK+CBrOr65T
 kFLLstCu9dtdJfvffyihaqiopq4nnhYxHpNntdwkxku9QDfx7s6PlRN5+EmgrM9LKIMjdYCz0AHg
 GSV6pRrsVNbt+tphsR9b6FdrJDdEeoSQhj50TMtJ3dPo07fvyoowBK5YiAv+TcxL3Hq79riGn+ge
 FFJ6oopfjCBiEW7E6GHS63w9agoW1Ex6n06n5pOi4xhE2KILUhDk3ANk0ToME63gKtZXCXx8Xvdl
 AfaRmrLkBIWGMkeQbOQDQdK0rL7MxgEmgT2lYY8KDulntOGHoOa/y1Ox0jWp6qY7mIEIIwk6JKFr
 UyxKHOV6YT1d9D/9eRBgDIOT2nGEQxtBRKCpkTkPkNe0pZqFcZlDfI4AgKTEaZojGDjNo6nyNli5
 7ivuLY719ILZ5ZEeG0yQnoW1oHMKYo6Um7bWLgrlNHqpG2Sy17QQHtEF/cYaQ0GEtTrCpd6WmLSa
 PITbyK2epy09ok+MY32Ps48PLWpx5RIAEhyMqjyrAoolsH60F8S1sfxzkrbWGrUysrU7T39fGZz5
 CsDbOhFSJztZZVpzbVrYEIPp0X0z3EDISyG7HOotYUw8/isMQwaSULu+b70MZ5wI7RnpHXB5G4us
 zQmCtPD31y48Fnt+GdmjPrn83lxiseA/xsl5P0afRwpeHCtri+Xb7wQ9CCnDCs+ezvBaNuo73vQD
 izPdHgI3JGTjhCNvORWsfn0Dyx9K4W4Yl4xBXKlr5IFvW+udnnDxvpbG/WLAQt42kzoetJX/x1C0
 e/FFEEHASoAvuYS5/9dyN/EeTeJI0qMJ329+9DNiPRFZDsGiU/YeXjQdEihrsMftvrzPZJ+Ey75e
 //wOtGNA7ja19TwAAAABJRU5ErkJggg==")
    ("\\`gosenjournalist\\'" . "\
Face: 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")
    ("\\`kiridoshi\\'" . "\
Face: iVBORw0KGgoAAAANSUhEUgAAADAAAAAwCAMAAABg3Am1AAAAulBMVEX///80gORbPjPalGo
 0o+zIimc1jufJeFhsqMs2mOjbrJFIsu+5bVK4p6G1X02EUEtyRDNJz/Y8Jx0neeWofVXewro9NTI
 rLzXOglkaFheGZ02HTTY7sPJx1/WriGk8puuHhIObUj4rhuQ9r+wsjObAgVUvQEw+t/IeJzEujeZ
 ktO4/vPY/xPpAwvdAw/dVcYifZFKgXkJBv/exlIHXhGZCwvhCw/dCxPgyiMvoyrxZUExaMyNDxfl
 Dxvahlyg1AAAAAXRSTlMAQObYZgAABGNJREFUSMeFlo12qjoQRj1QCmogkaJCYhHaotXeShBFBX3
 /17pfEm17Ts/PrHYtu7o3k8kkg73e93gcjc7n/fk8euz9Ox7Pm82m2ez3+/f35jIa/UN6PDSNEho
 tXJSA+MvTz+cDeND4vfGXZjn8PT863ITmsvYuzZXfLmez2fr3jwd/OFw8P+Vc1t56DX7/pvjZfPg
 n/uLVqeCCdVymte+Nrvzd3fx3/FnxUoiOMYGfKoKyNvyvxu35vsMZY1YXdh0sGjl27T3PtfCT8cH
 XrLMswGEYdpZal+M4meb7/f78F75pvJSDZLTKRaeMzqJR4UTeXPP91U8FHDbNpQbfdXlASAADuSw
 mIydNPcOv7q/C5XLQDfCiNuyECIjrujA61jGLKiPrG2E1vCXQLWs8bnUir4gS3CAHj7olT5FC44j
 PCpTkUQt7GYCO4xhGRxmFEKVIca/5l/ntBOkMWdcJPF8FHJdUFuU04jyN0qPhn55MgoPZpTJkVRW
 QIM9JrLIQ0bUtBB5J78o/fVmRFoTIA7eqiGPvYmVMLPDUCIp/+nE7FKoNZYilBNUuECGt4t0udkV
 oKYNzJTzp6I2u/GZzKbErkdztSFW5ThwjR96FVsspVcLLF+GABeGeldjVgNg7F0XHto0PAYSOUQv
 C0zV65w/hP49ij3Y2UhB3p4UK/Q5ZwL4J4HEnvQhnwsXa0QrF7wjDSaEuodnxmwB+66UuwUHK3SA
 wAv6skMyl5f3Lp2DW877dvq0jUjGWC4IicKyRSrUDibryI8HKZNi8v7+9va1lIHHR0Dfpuo5pBYq
 J8/Z4S7DqqwzowX4LYchzgc5VQcWErjoudAZW3n/wzz1VwWa/NYJQN1MIJjqsyXaMICdX4WXVn40
 wjZrNu06wXGYM9w1lu7FQRcRFUZAuTCZmj15Wd0uMwUddseKHmDCpPkxOTArHUXhBWJJMdQmGx9z
 EpFP829JLaxg0R4KdQ1zgBIJIkoVKsDK8EkwBy2GWZWlaRwFxYodUBDcbOQoIxxfEB98bGX7p6WP
 JpZQRkdhb3IwC+5on5Qr4ldeTfKvw5XLIM9/nGac8iiRXBiE4sYXv43r2r/z9pzB7XiQTnmWt1Ur
 JqajySjq2PRiP/ePxeOVXZoprfvY8TcIkweBoccc4lbJA6wan8XjQLsqh4R/MXNqaCf2aYEOyjHe
 My0gS/fjT6TROp4u2NPztNWEE8El5LHHvpOqzevwJRj2dThcLD+t/+HGblWqkz46JCaxfFlf+ZHh
 t3D88fE7vIRIckxA1JG3NctVns54PHnWUX98Pz/O7MmktCOmAV0XhDHy9/gwsom3bSZv99EZ5ni0
 sbk0p9Qd15Tg+Ynzy26kRFovJJPvlJTekdmRFdlHXforNH8OoF9PPyL69RgeYLDuHZmg4VjMe+Hz
 6Vx4Gbj6xkGGsE7RmNRpv//BVwLYLCOO0Hg/qVrGvZo+yP3/ZGOBNe1IJ2mmSvCKS1+m/vs8owW8
 VquLbv/8Hh23mKNbLy6sAAAAASUVORK5CYII=")
    ("\\`tokiura\\'" . "\
Face: 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
 yAAAAAElFTkSuQmCC")))

(defvar shimbun-yoshirin-expiration-days 1000)

(luna-define-method shimbun-groups ((shimbun shimbun-yoshirin))
  (mapcar 'car shimbun-yoshirin-group-table))

(luna-define-method shimbun-current-group-name ((shimbun shimbun-yoshirin))
  (nth 1 (assoc (shimbun-current-group-internal shimbun)
		shimbun-yoshirin-group-table)))

(luna-define-method shimbun-index-url ((shimbun shimbun-yoshirin))
  (nth 2 (assoc (shimbun-current-group-internal shimbun)
		shimbun-yoshirin-group-table)))

(luna-define-method shimbun-headers :around ((shimbun shimbun-yoshirin)
					     &optional range)
  ;; backward compat; see `shimbun-yoshirin-group-table'.
  (when (shimbun-index-url shimbun)
    (luna-call-next-method)))

(luna-define-method shimbun-get-headers ((shimbun shimbun-yoshirin)
					 &optional range)
  (shimbun-yoshirin-get-headers shimbun
				(or shimbun-yoshirin-index-range range)))

(luna-define-method shimbun-make-contents :around ((shimbun shimbun-yoshirin)
						   header)
  (shimbun-yoshirin-make-contents shimbun header)
  (let ((group (cdr (assq 'group (shimbun-header-extra header))))
	x-face ofn)
    (when (and group (assoc group shimbun-yoshirin-group-table))
      (setq x-face (or (cdr (assoc group shimbun-yoshirin-x-face-alist))
		       (catch 'face
			 (dolist (elem shimbun-yoshirin-x-face-alist)
			   (when (and (string-match "[]$*+\\^[]" (car elem))
				      (string-match (car elem) group))
			     (throw 'face (cdr elem)))))
		       shimbun-x-face))
      (setq ofn (symbol-function #'shimbun-x-face))
      (fset #'shimbun-x-face
	    `(lambda (shimbun)
	       (shimbun-set-x-face-internal shimbun ,x-face))))
    (unwind-protect
	(luna-call-next-method)
      (when ofn
	(fset #'shimbun-x-face ofn)))))

(luna-define-method shimbun-clear-contents :around ((shimbun shimbun-yoshirin)
						    header)
  t) ;; Force inserting footer.

(luna-define-method shimbun-footer :around ((shimbun shimbun-yoshirin)
					    header &optional html)
  (shimbun-yoshirin-footer shimbun header))

(defun shimbun-yoshirin-footer (shimbun header)
  (concat "<div align=\"left\">\n--&nbsp;<br>\n\
原物は<a href=\"" (shimbun-article-base-url shimbun header)
	  "\"><u>ここ</u></a>で公開されています。\n</div>\n"))

(defun shimbun-yoshirin-get-headers (shimbun range)
  "Get headers for articles in the group of SHIMBUN in RANGE."
  (let ((count (cond ((memq range '(0 nil all)) nil)
		     ((natnump range) range)
		     (t 1)))
	headers)
    (when (or (not count) (> count 0))
      (let (group from st nd id url subject datetime)
	(catch 'stop
	  (while t
	    (when (and (re-search-forward "\
<div\\(?:[\t\n ]+[^\t\n >]+\\)*[\t\n ]+class=\"main-content" nil t)
		       (shimbun-end-of-tag "div"))
	      (save-restriction
		(narrow-to-region (goto-char (match-beginning 2))
				  (match-end 2))
		(while (re-search-forward "\
<div\\(?:[\t\n ]+[^\t\n >]+\\)*[\t\n ]+class=\"swiper-slide\"[^>]*" nil t)
		  (goto-char (match-beginning 0))
		  (setq group (or (and (re-search-forward "\
[\t\n ]data-author=\"\\([^\"]+\\)" (match-end 0) t)
				       (match-string 1))
				  (shimbun-current-group-internal shimbun))
			from (or (and (nth 1 (assoc
					      group
					      shimbun-yoshirin-group-table)))
				 (shimbun-current-group-name shimbun)))
		  (when (shimbun-end-of-tag "div")
		    (goto-char (setq st (match-beginning 2)))
		    (setq nd (match-end 2))
		    (when (and (re-search-forward "\
<span\\(?:[\t\n ]+[^\t\n >]+\\)*[\t\n ]+class=\"title\"" nd t)
			       (shimbun-end-of-tag "span")
			       (progn
				 (goto-char (match-beginning 2))
				 (re-search-forward "\
<a\\(?:[\t\n ]+[^\t\n >]+\\)*[\t\n ]+href=\"\\([^\"]+/\\([0-9]+\\)\\)\
[^>]+>\\([^<]+\\)" (match-end 2) t)))
		      (setq id (concat "<" (match-string 2) "." group "%"
				       shimbun-yoshirin-top-level-domain ">"))
		      (when (save-match-data (shimbun-search-id shimbun id))
			(throw 'stop nil))
		      (setq url (match-string 1)
			    subject (match-string 3))
		      (goto-char st)
		      (setq datetime
			    (or (and (re-search-forward "\
<time\\(?:[\t\n ]+[^\t\n >]+\\)*[\t\n ]+datetime=\"\\([^\"]+\\)" nd t)
				     (condition-case nil
					 (parse-time-string (match-string 1))
				       (error nil)))
				(decode-time (current-time) 32400)))
		      (push (shimbun-create-header
			     0 subject from
			     (shimbun-make-date-string
			      (nth 5 datetime) (nth 4 datetime)
			      (nth 3 datetime)
			      (format "%02d:%02d:%02d"
				      (nth 2 datetime) (nth 1 datetime)
				      (nth 0 datetime)))
			     id "" 0 0 url (list (cons 'group group)))
			    headers)
		      (goto-char nd))))))
	    (when (and count (<= (setq count (1- count)) 0))
	      (throw 'stop nil))
	    (goto-char (point-min))
	    (if (and (re-search-forward "\
<a\\(?:[\t\n ]+[^\t\n >]+\\)*[\t\n ]+class=\"next page-numbers\"[^>]*>" nil t)
		     (progn
		       (goto-char (match-beginning 0))
		       (re-search-forward "[\t\n ]href=\"\\([^\"]+\\)"
					  (match-end 0) t)))
		(shimbun-retrieve-url (prog1
					  (match-string 1)
					(erase-buffer)))
	      (throw 'stop nil))))))
    headers))

(defun shimbun-yoshirin-make-contents (shimbun header)
  "Make an html article."
  (when (and (or (re-search-forward "\
<div\\(?:[\t\n ]+[^\t\n >]+\\)*[\t\n ]+class=\"content-wrapper\"" nil t)
		 (re-search-forward "\
<div\\(?:[\t\n ]+[^\t\n >]+\\)*[\t\n ]+class=\"entry-content" nil t))
	     (shimbun-end-of-tag "div"))
    (delete-region (goto-char (match-end 0)) (point-max))
    (insert "\n")
    (delete-region (goto-char (point-min)) (match-beginning 0))))

(provide 'sb-yoshirin)

;; sb-yoshirin.el ends here
