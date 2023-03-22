import json
import shutil
from math import ceil, sqrt
from pathlib import Path
import subprocess

from PIL import Image
import numpy as np

_DUNE = shutil.which("dune")
if _DUNE is None:
    raise RuntimeError("'dune' must be installed")


LUCIFER = "Lucifer"
SDES = "SDES"
SIMPLE_SP96 = "SimpleSP96"
ECB = "ECB"
CBC = "CBC"
CTR = "CTR"
ZERO = "Zero"
ANSI_X923 = "ANSI_X923"
PKCS7 = "PKCS7"
RFC_1321 = "RFC_1321"
ENC = "ENCRYPTION"
DEC = "DECRYPTION"

actual_dir = Path(__file__).resolve().parent

INPUT_FILE = actual_dir / "res" / "input.json"
OUTPUT_FILE = actual_dir / "res" / "output.json"

tbytes = tuple[int, bytes]


def tbytes_to_json(input: tbytes) -> dict[str, int]:
    bit_size = input[0]
    data_bytes = input[1]

    nb_of_chunks = ceil(bit_size / 32)

    int_list = []

    for i in range(nb_of_chunks):
        if 4*(i+1) >= len(data_bytes):
            end_part = len(data_bytes)
        else:
            end_part = 4*(i+1)

        int_list.append(int.from_bytes(
            data_bytes[4*i: end_part], byteorder='little'))

    return {"size": bit_size, "bytes": int_list}


def json_to_tbytes(input: dict[str, int]) -> tbytes:
    int_list: list[int] = input["bytes"]
    bytes_left: int = ceil(input["size"]/8)

    data_bytes = bytes()

    for value in int_list:
        if bytes_left >= 4:
            data_bytes += int.to_bytes(value, 4, byteorder='little')
        else:
            data_bytes += int.to_bytes(value, bytes_left, byteorder='little')

    return (input["size"], data_bytes)


def call_feistel(problem, cipher, mode, padding, iv: tbytes,
                 key: tbytes, data: tbytes) -> tbytes:
    if INPUT_FILE.exists():
        INPUT_FILE.unlink()

    if problem == ENC:
        do_encryption = True
    elif problem == DEC:
        do_encryption = False
    else:
        raise ValueError(f"{problem} is not a correct value.")

    iv_dict = tbytes_to_json(iv)
    key_dict = tbytes_to_json(key)
    data_dict = tbytes_to_json(data)

    input_data = {"cipher": cipher, "mode": mode,
                  "padding": padding, "do_encryption": do_encryption,
                  "iv": iv_dict, "key": key_dict, "data": data_dict}

    with INPUT_FILE.open("w") as f:
        json.dump(input_data, f)

    subprocess.check_call([_DUNE, "exec", "EncryptImg",
                          str(INPUT_FILE), str(OUTPUT_FILE)], cwd=actual_dir)

    with OUTPUT_FILE.open("r") as f:
        out_data = json.load(f)

    return json_to_tbytes(out_data)


def int_to_tbytes(integer, bit_size):
    return (bit_size, int.to_bytes(integer, ceil(bit_size/8), byteorder='little'))


def tbytes_from_output(outfile: Path) -> tbytes:
    with open(outfile, "r") as f:
        return json_to_tbytes(json.load(f))


def squared_img_to_tbytes(image_file) -> tuple[bool, tbytes]:
    img = Image.open(image_file)
    if img.mode not in ["RGB", "L"]:
        raise ValueError("Only RGB or Grayscale Img")

    img_array = np.array(img)
    img_bytes = img_array.tobytes()
    return (img.mode == "RGB", (len(img_bytes)*8, img_bytes))


def tbytes_to_squared_img(data: tbytes, image_file, rgb_mode=True) -> None:
    data_len = len(data[1])

    if rgb_mode:
        dim_img = int(sqrt(data_len / 3))
    else:
        dim_img = int(sqrt(data_len))

    if rgb_mode:
        img_data_len = dim_img*dim_img*3
    else:
        img_data_len = dim_img*dim_img

    img_array = np.frombuffer(
        buffer=data[1], dtype='uint8', count=img_data_len)

    if rgb_mode:
        img_array = img_array.reshape((dim_img, dim_img, 3))
        img = Image.fromarray(img_array, 'RGB')
    else:
        img_array = img_array.reshape((dim_img, dim_img))
        img = Image.fromarray(img_array, 'L')

    img = img.transpose(method=Image.Transpose.FLIP_TOP_BOTTOM).transpose(
        method=Image.Transpose.FLIP_LEFT_RIGHT)
    img.save(image_file)


def sdes_test(data_path: Path):
    is_rgb, data = squared_img_to_tbytes(data_path)

    key = int_to_tbytes(624, 10)
    cbc_iv = int_to_tbytes(181, 8)
    ctr_iv = int_to_tbytes(10, 4)

    print("ECB")
    out_ecb = call_feistel(ENC, SDES, ECB, RFC_1321, ctr_iv, key, data)
    print("CBC")
    out_cbc = call_feistel(ENC, SDES, CBC, RFC_1321, cbc_iv, key, data)
    print("CTR")
    out_ctr = call_feistel(ENC, SDES, CTR, RFC_1321, ctr_iv, key, data)

    data_dir = data_path.parent

    tbytes_to_squared_img(out_ecb, data_dir /
                          f"{data_path.stem}_encoded_sdes_ecb{data_path.suffix}", is_rgb)
    tbytes_to_squared_img(out_cbc, data_dir /
                          f"{data_path.stem}_encoded_sdes_cbc{data_path.suffix}", is_rgb)
    tbytes_to_squared_img(out_ctr, data_dir /
                          f"{data_path.stem}_encoded_sdes_ctr{data_path.suffix}", is_rgb)


def lucifer_test(data_path: Path):
    is_rgb, data = squared_img_to_tbytes(data_path)

    key = int_to_tbytes(262941950931828227601538166772406158918, 128)
    cbc_iv = int_to_tbytes(19705073246437364975993072628724612408, 128)
    ctr_iv = int_to_tbytes(17222837851353047736, 64)

    print("ECB")
    out_ecb = call_feistel(ENC, LUCIFER, ECB, RFC_1321, ctr_iv, key, data)
    print("CBC")
    out_cbc = call_feistel(ENC, LUCIFER, CBC, RFC_1321, cbc_iv, key, data)
    print("CTR")
    out_ctr = call_feistel(ENC, LUCIFER, CTR, RFC_1321, ctr_iv, key, data)

    data_dir = data_path.parent

    tbytes_to_squared_img(out_ecb, data_dir /
                          f"{data_path.stem}_encoded_lucifer_ecb{data_path.suffix}", is_rgb)
    tbytes_to_squared_img(out_cbc, data_dir /
                          f"{data_path.stem}_encoded_lucifer_cbc{data_path.suffix}", is_rgb)
    tbytes_to_squared_img(out_ctr, data_dir /
                          f"{data_path.stem}_encoded_lucifer_ctr{data_path.suffix}", is_rgb)


def simple_sp96_test(data_path: Path):
    is_rgb, data = squared_img_to_tbytes(data_path)

    key = int_to_tbytes(29843634044822567359336441712, 96)
    cbc_iv = int_to_tbytes(35094754211702257753073868467, 96)
    ctr_iv = int_to_tbytes(74282985796835, 48)

    print("ECB")
    out_ecb = call_feistel(ENC, SIMPLE_SP96, ECB,
                           RFC_1321, ctr_iv, key, data)
    print("CBC")
    out_cbc = call_feistel(ENC, SIMPLE_SP96, CBC,
                           RFC_1321, cbc_iv, key, data)
    print("CTR")
    out_ctr = call_feistel(ENC, SIMPLE_SP96, CTR,
                           RFC_1321, ctr_iv, key, data)

    data_dir = data_path.parent

    tbytes_to_squared_img(out_ecb, data_dir /
                          f"{data_path.stem}_encoded_sp96_ecb{data_path.suffix}", is_rgb)
    tbytes_to_squared_img(out_cbc, data_dir /
                          f"{data_path.stem}_encoded_sp96_cbc{data_path.suffix}", is_rgb)
    tbytes_to_squared_img(out_ctr, data_dir /
                          f"{data_path.stem}_encoded_sp96_ctr{data_path.suffix}", is_rgb)


def test_decrypt_sdes(data_path: Path, mode: str):
    is_rgb, data = squared_img_to_tbytes(data_path)

    key = int_to_tbytes(624, 10)
    if mode == CTR:
        iv = int_to_tbytes(10, 4)
    else:
        iv = int_to_tbytes(181, 8)

    out_ecb = call_feistel(DEC, SDES, mode,
                           RFC_1321, iv, key, data)

    data_dir = data_path.parent

    tbytes_to_squared_img(out_ecb, data_dir /
                          f"test_decoded{data_path.suffix}", is_rgb)


def main():
    in_img = actual_dir / ".." / "Resultats" / \
        "star" / "star.png"
    print("SDES")
    sdes_test(in_img)
    print("SP96")
    simple_sp96_test(in_img)
    print("Lucifer")
    lucifer_test(in_img)

    enc_img = actual_dir / ".." / "Resultats" / \
        "diffusion" / "diffusion_encoded_sdes_ecb.png"
    test_decrypt_sdes(enc_img, CTR)


if __name__ == "__main__":
    main()
