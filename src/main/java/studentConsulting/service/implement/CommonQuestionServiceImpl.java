package studentConsulting.service.implement;

import java.util.Optional;

import javax.transaction.Transactional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;

import studentConsulting.model.entity.questionAnswer.AnswerEntity;
import studentConsulting.model.entity.questionAnswer.CommonQuestionEntity;
import studentConsulting.model.entity.questionAnswer.QuestionEntity;
import studentConsulting.model.payload.dto.CommonQuestionDTO;
import studentConsulting.repository.AnswerRepository;
import studentConsulting.repository.CommonQuestionRepository;
import studentConsulting.repository.QuestionRepository;
import studentConsulting.service.ICommonQuestionService;

@Service
public class CommonQuestionServiceImpl implements ICommonQuestionService {

    @Autowired
    private CommonQuestionRepository commonQuestionRepository;

    @Autowired
    private QuestionRepository questionRepository;
    
    @Autowired
    private AnswerRepository answerRepository;

    @Override
    public Page<CommonQuestionDTO> getCommonQuestionsByDepartmentAndTitle(Integer departmentId, String title, Pageable pageable) {
        Page<CommonQuestionEntity> commonQuestions = commonQuestionRepository.findByDepartmentIdAndTitle(departmentId, title, pageable);
        return commonQuestions.map(this::mapToDTO);
    }

    @Override
    public Page<CommonQuestionDTO> getAllCommonQuestions(Pageable pageable) {
        Page<CommonQuestionEntity> commonQuestions = commonQuestionRepository.findAll(pageable);
        return commonQuestions.map(this::mapToDTO);
    }

    @Override
    public Page<CommonQuestionDTO> getCommonQuestionsByDepartment(Integer departmentId, Pageable pageable) {
        Page<CommonQuestionEntity> commonQuestions = commonQuestionRepository.findByDepartmentId(departmentId, pageable);
        return commonQuestions.map(this::mapToDTO);
    }

    @Override
    public Page<CommonQuestionDTO> searchCommonQuestionsByTitle(String title, Pageable pageable) {
        Page<CommonQuestionEntity> commonQuestions = commonQuestionRepository.findByTitle(title, pageable);
        return commonQuestions.map(this::mapToDTO);
    }

    private CommonQuestionDTO mapToDTO(CommonQuestionEntity question) {
        return CommonQuestionDTO.builder()
            .department(CommonQuestionDTO.DepartmentDTO.builder()
                .id(question.getDepartment().getId())
                .name(question.getDepartment().getName())
                .build())
            .field(CommonQuestionDTO.FieldDTO.builder()
                .id(question.getField().getId())
                .name(question.getField().getName())
                .build())
            .roleAsk(CommonQuestionDTO.RoleAskDTO.builder()
                .id(question.getRoleAsk().getId())
                .name(question.getRoleAsk().getName())
                .build())
            .title(question.getTitle())
            .content(question.getContent())
            .fileName(question.getFileName())
            .answerTitle(question.getAnswerTitle())
            .answerContent(question.getAnswerContent())
            .answerUserEmail(question.getAnswerUserEmail())
            .answerUserFirstname(question.getUser().getFirstName())
            .answerUserLastname(question.getUser().getLastName())
            .answerCreatedAt(question.getAnswerCreatedAt())
            .views(question.getViews())
            .createdAt(question.getCreatedAt())
            .askerFirstname(question.getUser().getFirstName())
            .askerLastname(question.getUser().getLastName())
            .build();
    }

    @Override
    @Transactional
    public CommonQuestionDTO convertToCommonQuestion(Integer questionId) {
        QuestionEntity question = questionRepository.findById(questionId)
                .orElseThrow(() -> new RuntimeException("Câu hỏi không tồn tại"));

        CommonQuestionEntity commonQuestion = new CommonQuestionEntity();
        commonQuestion.setTitle(question.getTitle());
        commonQuestion.setContent(question.getContent());
        commonQuestion.setFileName(question.getFileName());
        commonQuestion.setCreatedAt(question.getCreatedAt());
        commonQuestion.setViews(question.getViews());
        commonQuestion.setDepartment(question.getDepartment());
        commonQuestion.setField(question.getField());
        commonQuestion.setRoleAsk(question.getRoleAsk());
        commonQuestion.setUser(question.getUser());
        
        if (question.getUser() != null) {
            commonQuestion.setAskerFirstname(question.getUser().getFirstName());
            commonQuestion.setAskerLastname(question.getUser().getLastName());
        } else {
            commonQuestion.setAskerFirstname("Unknown");
            commonQuestion.setAskerLastname("Unknown");
        }

        Optional<AnswerEntity> firstAnswer = answerRepository.findFirstAnswerByQuestionId(question.getId());
        if (firstAnswer.isPresent()) {
            AnswerEntity answer = firstAnswer.get();
            commonQuestion.setAnswerContent(answer.getContent());
            
            if (answer.getUser() != null && answer.getUser().getAccount() != null) {
                commonQuestion.setAnswerUserEmail(answer.getUser().getAccount().getEmail());
                commonQuestion.setAnswerUserFirstname(answer.getUser().getFirstName());
                commonQuestion.setAnswerUserLastname(answer.getUser().getLastName());
            } else {
                commonQuestion.setAnswerUserEmail("unknown@example.com");
                commonQuestion.setAnswerUserFirstname("Unknown");
                commonQuestion.setAnswerUserLastname("Unknown");
            }
            commonQuestion.setAnswerCreatedAt(answer.getCreatedAt());
            commonQuestion.setAnswerTitle(answer.getTitle());
        }

        CommonQuestionEntity savedCommonQuestion = commonQuestionRepository.save(commonQuestion);

        return mapToDTO(savedCommonQuestion);
    }
}