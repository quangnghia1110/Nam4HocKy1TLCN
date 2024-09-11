package studentConsulting.service;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import studentConsulting.model.payload.dto.ConsultantDTO;
import studentConsulting.model.payload.dto.UserDTO;

public interface IConsultantService {
    Page<ConsultantDTO> getFilteredConsultants(Integer departmentId, String name, Pageable pageable);

    List<UserDTO> getConsultantsByDepartment(Integer departmentId);

}
