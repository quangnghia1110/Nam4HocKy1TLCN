package studentConsulting.service;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import studentConsulting.model.payload.dto.ConsultantDTO;
import studentConsulting.model.payload.dto.UserDTO;

public interface IConsultantService {
	public Page<ConsultantDTO> getAllConsultants(Pageable pageable);

	public Page<ConsultantDTO> getConsultantByDepartment(Integer departmentId, Pageable pageable);

	public Page<ConsultantDTO> searchConsultantsByName(String name, Pageable pageable);

	Page<ConsultantDTO> getConsultantsByDepartmentAndName(Integer departmentId, String firstName, Pageable pageable);

    List<UserDTO> getConsultantsByDepartment(Integer departmentId);

}
