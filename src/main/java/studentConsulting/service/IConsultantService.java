package studentConsulting.service;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import studentConsulting.model.payload.dto.ConsultantDTO;


public interface IConsultantService {
    public Page<ConsultantDTO> getAllConsultants(Pageable pageable);

	public Page<ConsultantDTO> getConsultantByDepartment(Integer departmentId, Pageable pageable);

	public Page<ConsultantDTO> searchConsultantsByName(String name, Pageable pageable);
}
