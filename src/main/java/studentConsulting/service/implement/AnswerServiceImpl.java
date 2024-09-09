package studentConsulting.service.implement;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.questionAnswer.AnswerEntity;
import studentConsulting.model.entity.questionAnswer.QuestionEntity;
import studentConsulting.model.entity.roleBaseAction.RoleConsultantEntity;
import studentConsulting.model.exception.CustomFieldErrorException;
import studentConsulting.model.exception.FieldErrorDetail;
import studentConsulting.model.payload.dto.AnswerDTO;
import studentConsulting.model.payload.request.answer.CreateAnswerRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.AnswerRepository;
import studentConsulting.repository.QuestionRepository;
import studentConsulting.repository.RoleConsultantRepository;
import studentConsulting.repository.UserRepository;
import studentConsulting.service.IAnswerService;

@Service
public class AnswerServiceImpl implements IAnswerService {

    @Autowired
    private AnswerRepository answerRepository;

    @Autowired
    private QuestionRepository questionRepository;

    @Autowired
    private RoleConsultantRepository roleConsultantRepository;

    @Autowired
    private UserRepository userInformationRepository;

	private static final String UPLOAD_DIR = "D:/HCMUTE-K21/DoAnGitHub/Nam4HocKy1TLCN/upload/"; // Đường dẫn lưu file

	public AnswerDTO createAnswer(CreateAnswerRequest request) {
	    List<FieldErrorDetail> errors = new ArrayList<>();

	    // Kiểm tra questionId
	    Optional<QuestionEntity> questionOpt = questionRepository.findById(request.getQuestionId());
	    if (questionOpt.isEmpty()) {
	        errors.add(new FieldErrorDetail("questionId", "Câu hỏi không tồn tại với ID: " + request.getQuestionId()));
	    }
	    if (!errors.isEmpty()) {
	        throw new CustomFieldErrorException(errors);
	    }

	    // Lấy đối tượng QuestionEntity từ Optional
	    QuestionEntity question = questionOpt.get();

	    // Kiểm tra xem câu hỏi đã có câu trả lời chưa
	    boolean hasAnswers = answerRepository.existsByQuestionId(request.getQuestionId());
	    
	    if (hasAnswers) {
	        errors.add(new FieldErrorDetail("questionId", "Câu hỏi này đã được trả lời, không thể trả lời lại."));
	    }
	   
	    // Kiểm tra roleConsultantId
	    Optional<RoleConsultantEntity> roleConsultant = roleConsultantRepository.findById(request.getRoleConsultantId());
	    if (roleConsultant.isEmpty()) {
	        errors.add(new FieldErrorDetail("roleConsultant", "Vai trò tư vấn không tồn tại"));
	    }

	    // Kiểm tra consultantId
	    Optional<UserInformationEntity> user = userInformationRepository.findById(request.getConsultantId());
	    if (user.isEmpty()) {
	        errors.add(new FieldErrorDetail("consultantId", "Người tư vấn không tồn tại với ID: " + request.getConsultantId()));
	    }

	    // Kiểm tra và lưu file nếu có file tải lên
	    String fileName = null;
	    if (request.getFile() != null && !request.getFile().isEmpty()) {
	        fileName = saveFile(request.getFile());
	    }

	    // Nếu có lỗi, ném ngoại lệ với danh sách lỗi
	    if (!errors.isEmpty()) {
	        throw new CustomFieldErrorException(errors);
	    }

	    if (request.getStatusApproval() != null && request.getStatusApproval()) {
	        // Thực hiện logic xử lý khi cần duyệt
	        errors.add(new FieldErrorDetail("statusApproval", "Cần kiểm duyệt câu trả lời này."));
	        return null;
	    } else {
	        // Tạo đối tượng AnswerEntity và lưu vào cơ sở dữ liệu
	        AnswerEntity answer = AnswerEntity.builder()
	            .question(question)
	            .roleConsultant(roleConsultant.get())
	            .user(user.get())
	            .title(request.getTitle())
	            .content(request.getContent())
	            .file(fileName)
	            .statusApproval(request.getStatusApproval()) 
	            .statusAnswer(false)
	            .createdAt(LocalDateTime.now())
	            .updatedAt(LocalDateTime.now())
	            .build();

	        AnswerEntity savedAnswer = answerRepository.save(answer);

	        // Sau khi tạo câu trả lời thành công, cập nhật trạng thái statusApproval trong QuestionEntity
	        question.setStatusApproval(true);
	        questionRepository.save(question);

	        return mapToAnswerDTO(savedAnswer);
	    }
	}



    // Hàm map đối tượng AnswerEntity sang AnswerDTO
    private AnswerDTO mapToAnswerDTO(AnswerEntity answer) {
        return AnswerDTO.builder()
                .questionId(answer.getQuestion().getId())
                .roleConsultantId(answer.getRoleConsultant().getId())
                .userId(answer.getUser().getId())
                .title(answer.getTitle())
                .content(answer.getContent())
                .file(answer.getFile())
                .createdAt(answer.getCreatedAt())
                .updatedAt(answer.getUpdatedAt())
                .statusApproval(answer.getStatusApproval())  // Đảm bảo truyền giá trị này
                .statusAnswer(answer.getStatusAnswer())
                .build();
    }

    // Hàm lưu file
    private String saveFile(MultipartFile file) {
        try {
            String fileName = file.getOriginalFilename();
            Path path = Paths.get(UPLOAD_DIR + fileName);

            // Kiểm tra và tạo thư mục nếu chưa tồn tại
            if (Files.notExists(path.getParent())) {
                Files.createDirectories(path.getParent());
            }

            // Lưu file vào đường dẫn đã định nghĩa
            Files.write(path, file.getBytes());

            return fileName;
        } catch (IOException e) {
            throw new RuntimeException("Could not store the file. Error: " + e.getMessage());
        }
    }
}
