package studentConsulting.service.interfaces.common;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import studentConsulting.model.payload.dto.actor.ConsultantDTO;
import studentConsulting.model.payload.dto.actor.UserDTO;

import java.time.LocalDate;
import java.util.List;

public interface IConsultantService {
    Page<ConsultantDTO> getFilteredConsultants(Integer departmentId, String name, LocalDate startDate,
                                               LocalDate endDate, Pageable pageable);

    List<UserDTO> getConsultantsByDepartment(Integer departmentId);

    List<UserDTO> getConsultantsTeacherByDepartment(Integer departmentId);

    List<UserDTO> getConsultantsStudentByDepartment(Integer departmentId);
}
