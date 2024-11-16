package studentConsulting.service.interfaces.common;

import org.springframework.http.ResponseEntity;
import studentConsulting.model.payload.response.DataResponse;

import java.util.List;

public interface IExportImportService {

    void importCommonQuestions(List<List<String>> csvData);

    void importManageConsultantSchedules(List<List<String>> csvData);

    void importAccounts(List<List<String>> csvData);

    void importAddresses(List<List<String>> csvData);

    void importDepartments(List<List<String>> csvData);

    void importDistricts(List<List<String>> csvData);

    void importFields(List<List<String>> csvData);

    void importProvinces(List<List<String>> csvData);

    void importRoleAsks(List<List<String>> csvData);

    void importRoleConsultants(List<List<String>> csvData);

    void importRoles(List<List<String>> csvData);

    void importUsers(List<List<String>> csvData);

    void importWards(List<List<String>> csvData);

    String getStringValue(Object obj);

    ResponseEntity<DataResponse> buildResponse(String message);

    String buildHeaderByPdf(Object item);

    String buildDataByPdf(List<?> items);
}
