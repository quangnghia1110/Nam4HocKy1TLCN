package studentConsulting.service.interfaces.common;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.payload.dto.actor.*;

import java.time.LocalDate;
import java.util.List;

public interface IGuestService {
    Page<ConsultantDTO> getConsultant(Integer departmentId, String name, LocalDate startDate, LocalDate endDate, Pageable pageable);

    List<UserDTO> getConsultantByDepartment(Integer departmentId);

    List<UserDTO> getConsultantTeacherByDepartment(Integer departmentId);

    List<UserDTO> getConsultantStudentByDepartment(Integer departmentId);

    Page<CommonQuestionDTO> getCommonQuestion(Integer departmentId, String title, LocalDate startDate, LocalDate endDate, Pageable pageable);

    List<DepartmentDTO> getAllDepartment();

    List<FieldDTO> getFieldByDepartment(Integer departmentId);

    Page<MyQuestionDTO> getQuestion(Integer departmentId, LocalDate startDate, LocalDate endDate, Pageable pageable);

    List<RoleAskDTO> getAllRoleAsk();

}
