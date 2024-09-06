package studentConsulting.service.implement;

import java.util.List;
import java.util.stream.Collectors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import studentConsulting.model.entity.questionAnswer.AnswerEntity;
import studentConsulting.model.entity.questionAnswer.CommonQuestionEntity;
import studentConsulting.model.payload.dto.CommonQuestionDTO;
import studentConsulting.repository.CommonQuestionRepository;
import studentConsulting.service.ICommonQuestionService;

@Service
public class CommonQuestionServiceImpl implements ICommonQuestionService {

    @Autowired
    private CommonQuestionRepository commonQuestionRepository;

    @Override
    public List<CommonQuestionDTO> getAllCommonQuestions() {
        List<CommonQuestionEntity> commonQuestions = commonQuestionRepository.findAll();  // Truy vấn danh sách câu hỏi

        return commonQuestions.stream()
            .map(this::mapToDTO) 
            .collect(Collectors.toList());
    }

    @Override
    public List<CommonQuestionDTO> getCommonQuestionsByDepartment(Integer departmentId) {
        List<CommonQuestionEntity> commonQuestions = commonQuestionRepository.findByDepartmentId(departmentId);
        return commonQuestions.stream()
            .map(this::mapToDTO)
            .collect(Collectors.toList());
    }

    @Override
    public List<CommonQuestionDTO> searchCommonQuestionsByTitle(String title) {
        List<CommonQuestionEntity> commonQuestions = commonQuestionRepository.findByTitle(title);
        return commonQuestions.stream()
            .map(this::mapToDTO)
            .collect(Collectors.toList());
    }

    // Hàm ánh xạ từ Entity sang DTO
    private CommonQuestionDTO mapToDTO(CommonQuestionEntity question) {
        return CommonQuestionDTO.builder()
            .departmentId(question.getDepartment().getId())
            .fieldId(question.getField().getId())
            .roleAskId(question.getRoleAsk().getId())
            .title(question.getTitle())
            .content(question.getContent())
            .fileName(question.getFileName())
            .contentAnswer(question.getContentAnswer())
            .status(question.getStatus() != null && question.getStatus() == 1) 
            .views(question.getViews())
            .createdAt(question.getCreatedAt())
            .updatedAt(question.getUpdatedAt())
            .build();
    }
}

