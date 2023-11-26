
import pandas as pd
from scipy import stats

def desc(df, drop = None, digits = 2):
    """
    对DataFrame中的数值列进行描述性统计。
    :param df: Pandas DataFrame，包含要分析的数据。
    :return: 描述性统计结果的DataFrame。
    """
    # 选择数值型列
    numeric_cols = df.select_dtypes(include='number').columns


    # 对整个DataFrame进行统计
    stats_df = df[numeric_cols].agg(['count','min', 'max', 'mean']).T
    mode_df = df[numeric_cols].apply(lambda x: stats.mode(x.dropna())[0])
    mode_df.name = 'mode'

    # 合并结果
    result = pd.concat([stats_df, mode_df],axis=1)
    result['count'] = result['count'].astype(int)


    if drop is not None:
        # 如果drop是字符串，将其转换为列表
        if isinstance(drop, str):
            drop = [drop]

        # 构建过滤条件
        mask = ~result.index.to_series().apply(lambda x: any(d in str(x) for d in drop))
        
        # 应用过滤条件
        result = result[mask]

    return result.round(digits)


def safe_merge(df1, df2, how = 'left', on=None, left_on=None, right_on=None, **kwargs):
    print(f'Merge的方式是{how=}')
    # 确定键列
    if on is not None:
        key_columns = on
    elif left_on is not None and right_on is not None:
        key_columns = left_on if isinstance(left_on, list) else [left_on]
    else:
        key_columns = [col for col in df1.columns if col in df2.columns]

    # 执行合并
    merged_df = pd.merge(df1, df2, on=on, how = how, left_on=left_on, right_on=right_on, **kwargs)

    # 检查是否有重复
    duplicates = merged_df.duplicated(subset=key_columns)
    if duplicates.any():
        num_duplicates = duplicates.sum()
        first_duplicate = merged_df.loc[duplicates, key_columns].iloc[0]
        print(f"注意：合并后的 DataFrame 有 {num_duplicates}/{len(merged_df)} 个重复键值。第一个重复键值为: {first_duplicate}")
    
    return merged_df


class DataRenamer():
    """ 根据var_list_file中short_name和year，获取对应重命名表，对df进行重命名
    """
    def __init__(self, var_list_file):
        self.var_list = pd.read_excel(var_list_file)

    def rename(self,df,year):
        """按年重命名"""
        print("重命名信息:")
        rename_info = dict(zip(self.var_list[year], self.var_list['short_name']))
        print(rename_info)
        
        return df.rename(columns=rename_info)

    def get_var_list(self):
        return self.var_list

    def select(self, df):
        """选择表中存在的列，df需要重命名后"""
        cols = [c for c in df.columns if c in list(self.var_list['short_name'])]
        print('选择的列:')
        print(cols)
        return df[cols].copy()
    
    def rename_and_select(self,df,year):
        return self.select(self.rename(df,year))

