package studentConsulting.service;

import java.time.LocalDate;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;

import studentConsulting.model.payload.dto.CommonQuestionDTO;

public interface ICommonQuestionService {
    public Page<CommonQuestionDTO> getCommonQuestionsWithFilters(Integer departmentId,String title, LocalDate startDate, LocalDate endDate, Pageable pageable);

	public CommonQuestionDTO convertToCommonQuestion(Integer questionId);

}