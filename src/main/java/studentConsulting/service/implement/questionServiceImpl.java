package studentConsulting.service.implement;

import java.util.List;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;

import studentConsulting.entity.main.questionEntity;
import studentConsulting.repository.main.questionRepository;
import studentConsulting.request.main.questionRequest;
import studentConsulting.response.apiResponse;

@Service
public class questionServiceImpl {
	@Autowired
	private questionRepository questionRepository; 
	public apiResponse<Object> getAllQuestion(){
		List<questionEntity> listQuestion = questionRepository.findAll();
		return apiResponse.builder().status(200).message("Danh sách câu hỏi").data(listQuestion).build();
	}
	public apiResponse<Object> addQuestion(questionRequest questionRequest){
		questionEntity questionEntity = questionRepository.findByTitle(questionRequest.getTitle());
		if(questionEntity !=null) {
			return apiResponse.builder().status(101).message("Tiêu đề câu hỏi này đã tồn tại").build();
		}
		questionEntity = questionEntity.builder().title(questionRequest.getTitle()).build();
		questionRepository.save(questionEntity);
		return apiResponse.builder().status(200).message("Thêm câu hỏi thành công").data(questionEntity).build();
		
	}
	public apiResponse<Object> updateQuestion(Integer id, questionRequest questionRequest){
		Optional<questionEntity> questionEntity = questionRepository.findById(id);
		if(!questionEntity.isPresent()) {
			return apiResponse.builder().status(101).message("Không tìm thấy câu hỏi").build();
		}
		//Thêm điều kiện nếu được trả lời rồi thì không được xóa
		questionEntity.get().setTitle(questionRequest.getTitle());
		questionRepository.save(questionEntity.get());
		return apiResponse.builder().status(200).message("Chỉnh sửa câu hỏi thành công").data(questionEntity).build();
		
	}
	public apiResponse<Object> deleteQuestion(@PathVariable("id") Integer id){
		Optional<questionEntity> questionEntity = questionRepository.findById(id);
        if(!questionEntity.isPresent())
        {
            return apiResponse.builder().status(404).message("Không tìm thấy câu hỏi").build();
        }
        //Thêm điều kiện nếu được trả lời rồi thì không được xóa
        questionRepository.delete(questionEntity.get());
        return apiResponse.builder().status(200).message("Xóa câu hỏi thành công").build();	
    }
}
