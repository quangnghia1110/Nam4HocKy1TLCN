package studentConsulting.service;

import java.util.List;

import org.springframework.stereotype.Service;

import studentConsulting.model.payload.dto.ConsultantDTO;


public interface IConsultantService {
	public List<ConsultantDTO> getAllConsultants();
    public List<ConsultantDTO> getConsultantByDepartment(Integer departmentId);
    List<ConsultantDTO> searchConsultantsByName(String name);
}
