package studentConsulting.service;

import java.time.LocalDate;

import org.springframework.data.domain.Page;

import studentConsulting.model.payload.dto.AnswerDTO;
import studentConsulting.model.payload.request.answer.CreateAnswerRequest;
import studentConsulting.model.payload.request.answer.ReviewAnswerRequest;

public interface IAnswerService {
    public AnswerDTO createAnswer(CreateAnswerRequest request);
	public AnswerDTO reviewAnswer(ReviewAnswerRequest request);
    public Page<AnswerDTO> getApprovedAnswersByDepartmentWithFilters(Integer departmentId, LocalDate startDate, LocalDate endDate, int page, int size, String sortBy, String sortDir);
    public Page<AnswerDTO> getAllAnswersByDepartmentWithFilters(Integer departmentId, LocalDate startDate, LocalDate endDate, int page, int size, String sortBy, String sortDir);

}
