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

 // Lấy tất cả câu hỏi chung với phân trang
    @Override
    public Page<CommonQuestionDTO> getAllCommonQuestions(Pageable pageable) {
        Page<CommonQuestionEntity> commonQuestions = commonQuestionRepository.findAll(pageable);
        return commonQuestions.map(this::mapToDTO);
    }

    // Lấy câu hỏi chung theo phòng ban với phân trang
    @Override
    public Page<CommonQuestionDTO> getCommonQuestionsByDepartment(Integer departmentId, Pageable pageable) {
        Page<CommonQuestionEntity> commonQuestions = commonQuestionRepository.findByDepartmentId(departmentId, pageable);
        return commonQuestions.map(this::mapToDTO);
    }

    // Tìm kiếm câu hỏi chung theo tiêu đề với phân trang
    @Override
    public Page<CommonQuestionDTO> searchCommonQuestionsByTitle(String title, Pageable pageable) {
        Page<CommonQuestionEntity> commonQuestions = commonQuestionRepository.findByTitle(title, pageable);
        return commonQuestions.map(this::mapToDTO);
    }

    // Hàm ánh xạ từ CommonQuestionEntity sang CommonQuestionDTO
 // Hàm ánh xạ từ CommonQuestionEntity sang CommonQuestionDTO
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

    // Chuyển câu hỏi từ QuestionEntity sang CommonQuestionEntity
    @Override
    @Transactional
    public CommonQuestionDTO convertToCommonQuestion(Integer questionId) {
        // Lấy câu hỏi từ bảng câu hỏi (QuestionEntity)
        QuestionEntity question = questionRepository.findById(questionId)
                .orElseThrow(() -> new RuntimeException("Câu hỏi không tồn tại"));

        // Tạo đối tượng CommonQuestionEntity
        CommonQuestionEntity commonQuestion = new CommonQuestionEntity();
        commonQuestion.setTitle(question.getTitle());
        commonQuestion.setContent(question.getContent());
        commonQuestion.setFileName(question.getFileName());
        commonQuestion.setCreatedAt(question.getCreatedAt());
        commonQuestion.setViews(question.getViews());
        commonQuestion.setDepartment(question.getDepartment());
        commonQuestion.setField(question.getField());
        commonQuestion.setRoleAsk(question.getRoleAsk());

        // Lấy và gán thông tin người hỏi (asker)
        commonQuestion.setAskerFirstname(question.getUser().getFirstName());
        commonQuestion.setAskerLastname(question.getUser().getLastName());

        // Kiểm tra và lấy thông tin câu trả lời đầu tiên
        Optional<AnswerEntity> firstAnswer = answerRepository.findFirstAnswerByQuestionId(question.getId());
        if (firstAnswer.isPresent()) {
            AnswerEntity answer = firstAnswer.get();
            commonQuestion.setAnswerContent(answer.getContent());
            commonQuestion.setAnswerUserEmail(answer.getUser().getAccount().getEmail());
            commonQuestion.setAnswerCreatedAt(answer.getCreatedAt());
            commonQuestion.setAnswerTitle(answer.getTitle());

            // Lấy và gán thông tin người trả lời (answerer)
            commonQuestion.setAnswerUserFirstname(answer.getUser().getFirstName());
            commonQuestion.setAnswerUserLastname(answer.getUser().getLastName());
        }

        // Lưu câu hỏi chung vào cơ sở dữ liệu
        CommonQuestionEntity savedCommonQuestion = commonQuestionRepository.save(commonQuestion);

        // Trả về DTO của câu hỏi chung vừa được lưu
        return mapToDTO(savedCommonQuestion);
    }
}
