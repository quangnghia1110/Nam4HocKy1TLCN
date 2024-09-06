package studentConsulting.service;

import java.util.List;

import studentConsulting.model.payload.dto.CommonQuestionDTO;

public interface ICommonQuestionService {
	public List<CommonQuestionDTO> getAllCommonQuestions();

	public List<CommonQuestionDTO> getCommonQuestionsByDepartment(Integer departmentId);

	public List<CommonQuestionDTO> searchCommonQuestionsByTitle(String title);

}
