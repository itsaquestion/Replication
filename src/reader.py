import os
import re
import pandas as pd


def read_data(file_path, size_threshold_m=10): # 默认阈值为10MB
    """
    按扩展名读取数据，返回df。如果>10M，则保存一个文件名+'.feather.
    """
    # 检查 file_path 是否存在
    if not os.path.exists(file_path):
        raise FileNotFoundError(f"文件 {file_path} 不存在。")

    print(file_path)

    feather_path = file_path + '.feather'
    size_threshold = size_threshold_m *1024*1024

    # 检查 feather 文件是否存在
    if os.path.exists(feather_path):
        print('feather存在，读取feather。')
        return pd.read_feather(feather_path)
    else:
        # 检查原始文件大小
        print('feather不存在，读取原数据。')
        file_size = os.path.getsize(file_path)

        # 根据文件扩展名选择读取方法
        file_extension = os.path.splitext(file_path)[1].lower()
        if file_extension == '.csv':
            data = pd.read_csv(file_path)
        elif file_extension in ['.xlsx', '.xls']:
            data = pd.read_excel(file_path)
        elif file_extension == '.dta':
            data = pd.read_stata(file_path, convert_categoricals=False)
        elif file_extension == '.sas7bdat':
            data = pd.read_sas(file_path)
        else:
            raise ValueError(f"不支持的文件格式：{file_extension}")
        
        # 如果文件大小超过阈值，则保存为 feather 文件
        if file_size > size_threshold:
            data.to_feather(feather_path)

        return data
    

def read_cfps(file_path):
    """用read_data读取cfps数据，cyear保存为wav"""
    df = read_data(file_path)

    df.columns = [s.replace('cfps_','') for s in df.columns]

    # 从文件名提取年份
    filename = os.path.basename(file_path)
    match = re.search(r'cfps(\d{4})', filename)
    if match:
        # 提取年份并添加到 'wav' 列
        #print(int(match.group(1)))
        df['wav'] = int(match.group(1))

    else:
        # 如果文件名中没有年份，抛出错误
        raise ValueError("无法从文件名中提取年份")

    return df



def find_files(directory, regex):
    """
    在指定的目录及其子目录中查找符合正则表达式的文件。

    参数:
    directory (str): 要搜索的目录路径。
    regex (str): 用于匹配文件名的正则表达式。

    返回:
    list: 符合条件的文件的完整路径列表。
    """
    matched_files = []
    compiled_regex = re.compile(regex)

    for root, dirs, files in os.walk(directory):
        for file in files:
            if compiled_regex.match(file):
                matched_files.append(os.path.join(root, file))

    return matched_files

def find_cfps(year,db,data_path=os.getenv('data_path')):
    return find_files(data_path + '/cfps', f'.*{year}.*{db}.*\.dta$')
