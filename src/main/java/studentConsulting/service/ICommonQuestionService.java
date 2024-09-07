package studentConsulting.service;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import studentConsulting.model.payload.dto.CommonQuestionDTO;

public interface ICommonQuestionService {
    public Page<CommonQuestionDTO> getCommonQuestionsByDepartmentAndTitle(Integer departmentId, String title, Pageable pageable);
    
	public Page<CommonQuestionDTO> getAllCommonQuestions(Pageable pageable);

	public Page<CommonQuestionDTO> getCommonQuestionsByDepartment(Integer departmentId, Pageable pageable);

	public Page<CommonQuestionDTO> searchCommonQuestionsByTitle(String title, Pageable pageable);

	public CommonQuestionDTO convertToCommonQuestion(Integer questionId);

}
