package studentConsulting.service.interfaces.admin;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.payload.dto.manage.ManageFieldDTO;
import studentConsulting.model.payload.request.FieldRequest;

import java.util.List;

public interface IAdminFieldService {
    ManageFieldDTO createField(Integer departmentId, FieldRequest fieldRequest);

    ManageFieldDTO updateField(Integer id, Integer departmentId, FieldRequest fieldRequest);

    void deleteFieldById(Integer id);

    ManageFieldDTO getFieldById(Integer id);

    Page<ManageFieldDTO> getAllFieldsWithFilters(String name, String departmentId, Pageable pageable);

    boolean existsById(Integer id);

    void importFields(List<List<String>> csvData);

    String getDepartmentNameById(Integer departmentId);

}

