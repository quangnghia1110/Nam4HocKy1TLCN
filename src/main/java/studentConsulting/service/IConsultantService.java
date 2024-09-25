package studentConsulting.service;

import java.security.Principal;
import java.time.LocalDate;
import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import studentConsulting.model.payload.dto.ConsultantDTO;
import studentConsulting.model.payload.dto.ManageUserInformationDTO;
import studentConsulting.model.payload.dto.UserDTO;

public interface IConsultantService {
	public Page<ConsultantDTO> getFilteredConsultants(Integer departmentId, String name, LocalDate startDate,
			LocalDate endDate, Pageable pageable);

	List<UserDTO> getConsultantsByDepartment(Integer departmentId);

	public List<UserDTO> getConsultantsTeacherByDepartment(Integer departmentId);

	public List<UserDTO> getConsultantsStudentByDepartment(Integer departmentId);

	public Page<ManageUserInformationDTO> getConsultantsByManagerWithFilters(LocalDate startDate, LocalDate endDate,
			Pageable pageable, Principal principal);

	public void updateConsultantRoleToUser(Integer id, Principal principal);
}
