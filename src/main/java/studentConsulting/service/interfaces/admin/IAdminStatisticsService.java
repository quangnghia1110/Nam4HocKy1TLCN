package studentConsulting.service.interfaces.admin;

import studentConsulting.model.payload.dto.statistic.AdminStatisticsDTO;

import java.util.List;
import java.util.Map;

public interface IAdminStatisticsService {

    AdminStatisticsDTO getAllAdminStatistics();

    List<Map<String, Object>> getAccountsByYear(Integer year);

    List<Map<String, Object>> getDepartmentsByYear(Integer year);

    List<Map<String, Object>> getFieldsByYear(Integer departmentId, Integer year);

    List<Map<String, Object>> getRoleAsksByYear(Integer year);

    List<Map<String, Object>> getRoleConsultantsByYear(Integer year);

    List<Map<String, Object>> getRolesByYear(Integer year);

//    List<Map<String, Object>> getAddressesByYear(Integer year);
//
//    List<Map<String, Object>> getProvincesByYear(Integer year);
//
//    List<Map<String, Object>> getDistrictsByYear(Integer year);
//
//    List<Map<String, Object>> getWardsByYear(Integer year);
}
