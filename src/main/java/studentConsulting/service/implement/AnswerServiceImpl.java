package studentConsulting.service.implement;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import com.cloudinary.Cloudinary;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.questionAnswer.AnswerEntity;
import studentConsulting.model.entity.questionAnswer.QuestionEntity;
import studentConsulting.model.entity.roleBaseAction.RoleConsultantEntity;
import studentConsulting.model.exception.CustomFieldErrorException;
import studentConsulting.model.exception.FieldErrorDetail;
import studentConsulting.model.payload.dto.AnswerDTO;
import studentConsulting.model.payload.request.answer.CreateAnswerRequest;
import studentConsulting.repository.AnswerRepository;
import studentConsulting.repository.QuestionRepository;
import studentConsulting.repository.RoleConsultantRepository;
import studentConsulting.repository.UserRepository;
import studentConsulting.service.IAnswerService;

@Service
public class AnswerServiceImpl implements IAnswerService {

	@Autowired
	private Cloudinary cloudinary;
	
    @Autowired
    private AnswerRepository answerRepository;

    @Autowired
    private QuestionRepository questionRepository;

    @Autowired
    private RoleConsultantRepository roleConsultantRepository;

    @Autowired
    private UserRepository userInformationRepository;

	public AnswerDTO createAnswer(CreateAnswerRequest request) {
	    List<FieldErrorDetail> errors = new ArrayList<>();

	    Optional<QuestionEntity> questionOpt = questionRepository.findById(request.getQuestionId());
	    if (questionOpt.isEmpty()) {
	        errors.add(new FieldErrorDetail("questionId", "Câu hỏi không tồn tại với ID: " + request.getQuestionId()));
	    }
	    if (!errors.isEmpty()) {
	        throw new CustomFieldErrorException(errors);
	    }

	    QuestionEntity question = questionOpt.get();

	    boolean hasAnswers = answerRepository.existsByQuestionId(request.getQuestionId());

	    if (hasAnswers) {
	        errors.add(new FieldErrorDetail("questionId", "Câu hỏi này đã được trả lời, không thể trả lời lại."));
	    }

	    Optional<RoleConsultantEntity> roleConsultant = roleConsultantRepository.findById(request.getRoleConsultantId());
	    if (roleConsultant.isEmpty()) {
	        errors.add(new FieldErrorDetail("roleConsultant", "Vai trò tư vấn không tồn tại"));
	    }

	    Optional<UserInformationEntity> user = userInformationRepository.findById(request.getConsultantId());
	    if (user.isEmpty()) {
	        errors.add(new FieldErrorDetail("consultantId", "Người tư vấn không tồn tại với ID: " + request.getConsultantId()));
	    }

	    String fileName = null;
	    if (request.getFile() != null && !request.getFile().isEmpty()) {
	        fileName = saveFile(request.getFile());
	    }

	    if (!errors.isEmpty()) {
	        throw new CustomFieldErrorException(errors);
	    }

	    AnswerEntity answer = AnswerEntity.builder()
	        .question(question)
	        .roleConsultant(roleConsultant.get())
	        .user(user.get())
	        .title(request.getTitle())
	        .content(request.getContent())
	        .file(fileName)
	        .statusApproval(request.getStatusApproval())
	        .statusAnswer(true)
	        .createdAt(LocalDateTime.now())
	        .updatedAt(LocalDateTime.now())
	        .build();
	    question.setStatusApproval(true);
        questionRepository.save(question);

	    AnswerEntity savedAnswer = answerRepository.save(answer);

	    if (request.getStatusApproval() != null && request.getStatusApproval()) {
	        question.setStatusApproval(false); 
	        answer.setStatusAnswer(false); 
	        questionRepository.save(question);
	        return mapToAnswerDTO(savedAnswer); 
	    }

	    return mapToAnswerDTO(savedAnswer);
	}


	public AnswerDTO reviewAnswer(CreateAnswerRequest request) {
	    List<FieldErrorDetail> errors = new ArrayList<>();

	    Optional<AnswerEntity> answerOpt = answerRepository.findFirstAnswerByQuestionId(request.getQuestionId());
	    if (answerOpt.isEmpty()) {
	        errors.add(new FieldErrorDetail("answerId", "Câu trả lời không tồn tại."));
	        throw new CustomFieldErrorException(errors);
	    }

	    AnswerEntity answer = answerOpt.get();

	    String fileName = null;
	    if (request.getFile() != null && !request.getFile().isEmpty()) {
	        fileName = saveFile(request.getFile());
	        answer.setFile(fileName);
	    }

	    answer.setContent(request.getContent());
	    answer.setUpdatedAt(LocalDateTime.now());
	    answer.setStatusApproval(true); 
	    answer.setStatusAnswer(true); 
	    
	    Optional<QuestionEntity> questionOpt = questionRepository.findById(request.getQuestionId());
	    if (questionOpt.isEmpty()) {
	        errors.add(new FieldErrorDetail("questionId", "Câu hỏi không tồn tại với ID: " + request.getQuestionId()));
	    }
	    
	    QuestionEntity question = questionOpt.get();
	     question.setStatusApproval(true);
	        questionRepository.save(question);
	    AnswerEntity reviewedAnswer = answerRepository.save(answer);

	    return mapToAnswerDTO(reviewedAnswer);
	}

    public AnswerDTO mapToAnswerDTO(AnswerEntity answer) {
        return AnswerDTO.builder()
                .questionId(answer.getQuestion().getId())
                .roleConsultantId(answer.getRoleConsultant().getId())
                .userId(answer.getUser().getId())
                .title(answer.getTitle())
                .content(answer.getContent())
                .file(answer.getFile())
                .createdAt(answer.getCreatedAt())
                .updatedAt(answer.getUpdatedAt())
                .statusApproval(answer.getStatusApproval())  
                .statusAnswer(answer.getStatusAnswer())
                .build();
    }

    private String saveFile(MultipartFile file) {
	    try {
	        String fileType = file.getContentType();

	        if (fileType != null && fileType.startsWith("image")) {
	            Map<String, Object> uploadResult = cloudinary.uploader().upload(file.getBytes(), Map.of());
	            String fileUrl = (String) uploadResult.get("url");
	            return fileUrl; 
	        } else {
	            String uploadDir = "D:\\HCMUTE-K21\\DoAnGitHub\\Nam4HocKy1TLCN\\upload";
	            Path uploadPath = Paths.get(uploadDir);

	            if (!Files.exists(uploadPath)) {
	                Files.createDirectories(uploadPath);
	            }

	            String originalFileName = file.getOriginalFilename();
	            Path filePath = uploadPath.resolve(originalFileName);

	            Files.write(filePath, file.getBytes());

	            return filePath.toString();  
	        }
	    } catch (IOException e) {
	        throw new RuntimeException("Could not store the file. Error: " + e.getMessage());
	    }
	}
}